//! Extracts command ownership and executable semantic spans for differential comparison.

use tree_sitter::{Node, Tree, TreeCursor};

use crate::model::{Argument, Command, Diagnostic, Projection, Role, Semantic, Span};

/// Extracts command owners, executable roles and syntax diagnostics without recursion.
pub fn project(tree: &Tree) -> Projection {
    let mut projection = Projection {
        errors: Vec::new(),
        commands: Vec::new(),
        semantic: Vec::new(),
    };
    let mut cursor = tree.walk();
    loop {
        let node = cursor.node();
        if node.is_error() || node.is_missing() {
            projection.errors.push(Diagnostic {
                id: node.kind().to_owned(),
                start: node.start_byte(),
                end: node.end_byte(),
                incomplete: node.is_missing(),
            });
        }
        if node.kind() == "command" {
            projection.commands.push(project_command(node));
        }
        if let Some(role) = semantic_role(node.kind()) {
            let semantic = Semantic {
                role,
                start: node.start_byte(),
                end: node.end_byte(),
            };
            projection.semantic.push(semantic);
        }
        if !advance(&mut cursor) {
            break;
        }
    }
    projection
}

/// Canonicalizes order and removes duplicate method aliases.
pub fn normalize(projection: &mut Projection) {
    projection
        .commands
        .sort_by_key(|command| (command.start, command.end, command.name));
    projection.semantic.sort_unstable();
    projection.semantic.dedup_by(|a, b| a.role == Role::Method && a == b);
}

/// True when every oracle command matches its parser counterpart. Redirections and
/// non-`collapsed` arguments must match the oracle span exactly. A `collapsed`
/// argument may instead cover a gapped run of oracle spans, since the real `AST` has
/// no element for a separator the owner swallows, as long as the first and last
/// covered span still meet its own boundary exactly.
pub fn commands_agree(oracle: &[Command], parser: &[Command]) -> bool {
    oracle.len() == parser.len()
        && oracle.iter().zip(parser).all(|(reference, candidate)| {
            reference.start == candidate.start
                && reference.end == candidate.end
                && reference.name == candidate.name
                && reference.redirections == candidate.redirections
                && arguments_agree(&reference.arguments, &candidate.arguments)
        })
}

fn arguments_agree(oracle: &[Argument], parser: &[Argument]) -> bool {
    let mut oracle_spans = oracle.iter().map(|argument| argument.span).peekable();
    for argument in parser {
        if !argument.collapsed {
            if oracle_spans.next() != Some(argument.span) {
                return false;
            }
            continue;
        }
        let [start, end] = argument.span;
        let mut last_end = None;
        while let Some(&[span_start, span_end]) = oracle_spans.peek() {
            if span_start < start || span_end > end {
                break;
            }
            if last_end.is_none() && span_start != start {
                break;
            }
            oracle_spans.next();
            last_end = Some(span_end);
        }
        if last_end != Some(end) {
            return false;
        }
    }
    oracle_spans.next().is_none()
}

fn project_command(command: Node<'_>) -> Command {
    let start = command.start_byte();
    let name = match command.child_by_field_name("command_name") {
        Some(node) => [node.start_byte(), node.end_byte()],
        None => [start, start],
    };
    let mut end = name[1];
    let mut arguments: Vec<Argument> = Vec::new();
    let mut redirections: Vec<Span> = Vec::new();
    if let Some(elements) = command.child_by_field_name("command_elements") {
        for node in elements.named_children(&mut elements.walk()) {
            match node.kind() {
                "command_argument_sep" | "comment" => {}
                "redirection" => {
                    end = end.max(node.end_byte());
                    redirections.push([node.start_byte(), node.end_byte()]);
                }
                _ => {
                    end = end.max(node.end_byte());
                    if !(node.is_error() || node.is_missing()) {
                        arguments.push(Argument {
                            span: [node.start_byte(), node.end_byte()],
                            collapsed: node.kind() == "stop_parsing",
                        });
                    }
                }
            }
        }
    }
    Command {
        start,
        end,
        name,
        arguments,
        redirections,
    }
}

fn semantic_role(kind: &str) -> Option<Role> {
    match kind {
        "variable" => Some(Role::Variable),
        "sub_expression" => Some(Role::Subexpression),
        "invokation_expression" | "invokation_foreach_expression" => Some(Role::Method),
        "member_access" => Some(Role::Member),
        "parenthesized_expression" => Some(Role::Parentheses),
        "script_block_expression" => Some(Role::ScriptBlock),
        "assignment_expression" => Some(Role::Assignment),
        _ => None,
    }
}

/// Advances a preorder cursor without recursive traversal.
fn advance(cursor: &mut TreeCursor) -> bool {
    if cursor.goto_first_child() {
        return true;
    }
    while !cursor.goto_next_sibling() {
        if !cursor.goto_parent() {
            return false;
        }
    }
    true
}

#[cfg(test)]
mod tests {
    use super::*;
    use tree_sitter::Parser;

    fn parse(source: &str) -> Tree {
        let mut parser = Parser::new();
        parser
            .set_language(&tree_sitter_powershell::LANGUAGE.into())
            .expect("frozen main language loads");
        parser.parse(source, None).expect("parse completes")
    }

    fn command(
        start: usize,
        end: usize,
        name: Span,
        arguments: Vec<Argument>,
        redirections: Vec<Span>,
    ) -> Command {
        Command {
            start,
            end,
            name,
            arguments,
            redirections,
        }
    }

    fn arg(start: usize, end: usize) -> Argument {
        Argument { span: [start, end], collapsed: false }
    }

    fn collapsed_arg(start: usize, end: usize) -> Argument {
        Argument { span: [start, end], collapsed: true }
    }


    fn semantic(role: Role, start: usize, end: usize) -> Semantic {
        Semantic {
            role,
            start,
            end,
        }
    }

    fn assert_acceptance(tree: &Tree, projection: &Projection) {
        assert_eq!(
            !projection.errors.is_empty(),
            tree.root_node().has_error(),
            "diagnostics and root has-error acceptance disagree"
        );
    }

    #[test]
    fn simple_command_projects_name_and_argument_owners() {
        let source = "foo bar baz";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection,
            Projection {
                errors: vec![],
                commands: vec![command(0, 11, [0, 3], vec![arg(4, 7), arg(8, 11)], vec![])],
                semantic: vec![],
            }
        );
    }

    #[test]
    fn redirections_are_own_spans_and_extend_command_end() {
        let source = "Get-Content c:\\a.txt > out.txt 2> err.txt";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![command(0, 41, [0, 11], vec![arg(12, 20)], vec![[21, 30], [31, 41]])]
        );
        assert!(projection.semantic.is_empty());
        assert!(projection.errors.is_empty());
    }

    #[test]
    fn redirection_without_target_keeps_operator_span() {
        let source = "foo > ";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![command(0, 6, [0, 3], vec![], vec![[4, 6]])]
        );
    }

    #[test]
    fn multiple_commands_and_pipeline_chain_project_each_command() {
        let source = "foo bar; baz qux | quux";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![
                command(0, 7, [0, 3], vec![arg(4, 7)], vec![]),
                command(9, 16, [9, 12], vec![arg(13, 16)], vec![]),
                command(19, 23, [19, 23], vec![], vec![]),
            ]
        );
        assert!(projection.semantic.is_empty());
        assert!(projection.errors.is_empty());
    }

    #[test]
    fn astral_and_multibyte_arguments_use_utf8_byte_offsets() {
        let source = "Write-Host \u{e9} \u{1F441}";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        // é is 2 bytes and the astral eye is 4 bytes.
        assert_eq!(
            projection.commands,
            vec![command(0, 18, [0, 10], vec![arg(11, 13), arg(14, 18)], vec![])]
        );
    }

    #[test]
    fn quoted_multibyte_argument_owner_spans_whole_quote() {
        let source = "Write-Output \"h\u{e9}llo \u{1F441} world\"";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![command(0, 32, [0, 12], vec![arg(13, 32)], vec![])]
        );
        assert!(projection.semantic.is_empty());
    }

    #[test]
    fn invocation_operator_belonging_to_command_span() {
        let source = "& \"Get-ChildItem\" -Name x";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        // The operator belongs to the command span but not the name span.
        assert_eq!(
            projection.commands,
            vec![command(0, 25, [2, 17], vec![arg(18, 23), arg(24, 25)], vec![])]
        );
    }

    #[test]
    fn comment_and_separator_are_not_argument_owners() {
        let source = "foo <#c#> bar";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![command(0, 13, [0, 3], vec![arg(10, 13)], vec![])]
        );
    }

    #[test]
    fn stop_parsing_tail_is_an_argument_owner() {
        let source = "cmd --% literal stuff";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![command(0, 21, [0, 3], vec![collapsed_arg(4, 21)], vec![])]
        );
    }

    #[test]
    fn assignment_variable_and_parenthesis_spans_are_preserved() {
        let source = "$x = (1, 2)";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let mut projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert!(projection.commands.is_empty());
        normalize(&mut projection);
        assert_eq!(
            projection.semantic,
            vec![
                semantic(Role::Variable, 0, 2),
                semantic(Role::Parentheses, 5, 11),
                semantic(Role::Assignment, 0, 11),
            ]
        );
    }

    #[test]
    fn distinct_method_member_and_variable_spans_are_all_kept() {
        let source = "$x.ToUpper(); $y.Length";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let mut projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert!(projection.commands.is_empty());
        normalize(&mut projection);
        assert_eq!(
            projection.semantic,
            vec![
                semantic(Role::Variable, 0, 2),
                semantic(Role::Variable, 14, 16),
                semantic(Role::Method, 0, 12),
                semantic(Role::Member, 14, 23),
            ]
        );
    }

    #[test]
    fn same_span_method_alias_is_deduplicated() {
        let source = "@(1,2).foreach { Write-Host $_ }";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let mut projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![command(17, 30, [17, 27], vec![arg(28, 30)], vec![])]
        );
        normalize(&mut projection);
        assert_eq!(
            projection.semantic,
            vec![
                semantic(Role::Variable, 28, 30),
                semantic(Role::Method, 0, 32),
                semantic(Role::ScriptBlock, 15, 32),
            ]
        );
    }

    #[test]
    fn nested_command_inside_subexpression_projects_both_commands() {
        let source = "Write-Output $(foo bar)";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let projection = project(&tree);
        assert_acceptance(&tree, &projection);
        assert_eq!(
            projection.commands,
            vec![
                command(0, 23, [0, 12], vec![arg(13, 23)], vec![]),
                command(15, 22, [15, 18], vec![arg(19, 22)], vec![]),
            ]
        );
        assert_eq!(projection.semantic, vec![semantic(Role::Subexpression, 13, 23)]);
    }

    #[test]
    fn collapsed_argument_agrees_across_an_internal_separator_gap() {
        let oracle = vec![arg(4, 7), arg(8, 12)];
        let parser = vec![collapsed_arg(4, 12)];
        assert!(super::arguments_agree(&oracle, &parser));
    }

    #[test]
    fn collapsed_argument_rejects_a_wrong_leading_boundary_overrun_or_short_tail() {
        // Leading gap: the first covered element does not start at the collapsed boundary.
        assert!(!super::arguments_agree(&[arg(5, 7), arg(8, 12)], &[collapsed_arg(4, 12)]));
        // Overrun: the oracle's second span extends past the collapsed boundary.
        assert!(!super::arguments_agree(&[arg(4, 7), arg(8, 13)], &[collapsed_arg(4, 12)]));
        // Short tail: the collapsed span claims a trailing byte the oracle never covers.
        assert!(!super::arguments_agree(&[arg(4, 7), arg(8, 11)], &[collapsed_arg(4, 12)]));
        // A non-collapsed argument still requires an exact element-for-element match.
        assert!(!super::arguments_agree(&[arg(4, 7), arg(8, 12)], &[arg(4, 12)]));
    }

    #[test]
    fn stop_parsing_commands_agree_with_their_split_oracle_elements() {
        let source = "foo --% bar";
        let tree = parse(source);
        assert!(!tree.root_node().has_error());
        let parser = project(&tree).commands;
        let oracle = vec![command(0, 11, [0, 3], vec![arg(4, 7), arg(8, 11)], vec![])];
        assert_eq!(parser, vec![command(0, 11, [0, 3], vec![collapsed_arg(4, 11)], vec![])]);
        assert!(super::commands_agree(&oracle, &parser));
    }



}
