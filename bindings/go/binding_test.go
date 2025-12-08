package tree_sitter_powershell_test

import (
	"testing"

	tree_sitter "github.com/tree-sitter/go-tree-sitter"
	tree_sitter_powershell "github.com/airbus-cert/tree-sitter-powershell/bindings/go"
)

func TestCanLoadGrammar(t *testing.T) {
	language := tree_sitter.NewLanguage(tree_sitter_powershell.Language())
	if language == nil {
		t.Errorf("Error loading Powershell grammar")
	}
}
