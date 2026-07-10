; Multiline block comments
((comment) @fold
  (#match? @fold "^<#"))

; PowerShell editor region markers
((comment) @fold.start
  (#match? @fold.start "^#region\\b"))
((comment) @fold.end
  (#match? @fold.end "^#endregion\\b"))

; Script signature block markers
((comment) @fold.start
  (#match? @fold.start "^#\\s*SIG\\s+#\\s*Begin\\s+signature\\s+block\\b"))
((comment) @fold.end
  (#match? @fold.end "^#\\s*SIG\\s+#\\s*End\\s+signature\\s+block\\b"))
