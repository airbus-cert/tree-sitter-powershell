#Requires -Version 7.4

$ErrorActionPreference = 'Stop'
$env:POWERSHELL_TELEMETRY_OPTOUT = '1'
[Console]::InputEncoding = [System.Text.Encoding]::UTF8
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
$ProgressPreference = 'SilentlyContinue'
$VerbosePreference = 'SilentlyContinue'
$WarningPreference = 'SilentlyContinue'
$InformationPreference = 'SilentlyContinue'
$DebugPreference = 'SilentlyContinue'

# AST extents are UTF-16 code units; the harness compares UTF-8 bytes.
function New-Utf8ByteMap([string]$Text) {
    $map = New-Object 'int[]' ($Text.Length + 1)
    $bytes = 0
    for ($i = 0; $i -lt $Text.Length; $i++) {
        $map[$i] = $bytes
        $code = [int][char]$Text[$i]
        if ($code -ge 0xD800 -and $code -le 0xDBFF -and ($i + 1) -lt $Text.Length) {
            $next = [int][char]$Text[$i + 1]
            if ($next -ge 0xDC00 -and $next -le 0xDFFF) {
                $map[$i + 1] = $bytes + 3
                $bytes += 4
                $i++
                continue
            }
        }
        if ($code -ge 0x800) { $bytes += 3 }
        elseif ($code -ge 0x80) { $bytes += 2 }
        else { $bytes += 1 }
    }
    $map[$Text.Length] = $bytes
    $map
}

# A parser diagnostic at EOF can report an offset one past Text.Length; clamp into the map.
function Get-ByteOffset($Map, [int]$Offset) {
    if ($Offset -lt 0) { return $Map[0] }
    if ($Offset -ge $Map.Length) { return $Map[$Map.Length - 1] }
    $Map[$Offset]
}

function Write-Reply($Object) {
    [Console]::Out.WriteLine(($Object | ConvertTo-Json -Compress -Depth 16))
    [Console]::Out.Flush()
}

function Write-ErrorReply($Id, [string]$Message) {
    Write-Reply ([pscustomobject]@{ type = 'error'; id = $Id; message = $Message })
}

# JSON integers above Int64 arrive as BigInteger; normalize every id to uint64.
function Get-ReplyId($Request) {
    if ($null -eq $Request.PSObject.Properties['id']) { return $null }
    $value = $Request.id
    if ($null -eq $value) { return $null }
    if ($value -is [string]) {
        $parsed = [uint64]0
        if ([uint64]::TryParse($value, [ref]$parsed)) { return $parsed }
        return $null
    }
    if ($value -is [double] -and $value -ne [Math]::Floor($value)) { return $null }
    try { return [uint64]$value } catch { return $null }
}

function Get-Projection([string]$Source) {
    $map = New-Utf8ByteMap $Source
    $tokens = $null
    $errors = $null
    $ast = [System.Management.Automation.Language.Parser]::ParseInput($Source, [ref]$tokens, [ref]$errors)
    $diagnostics = [System.Collections.Generic.List[object]]::new()
    foreach ($err in $errors) {
        $diagnostics.Add([pscustomobject]@{
            id = [string]$err.ErrorId
            start = Get-ByteOffset $map $err.Extent.StartOffset
            end = Get-ByteOffset $map $err.Extent.EndOffset
            incomplete = [bool]$err.InCompleteInput
        })
    }
    $commands = [System.Collections.Generic.List[object]]::new()
    foreach ($cmd in $ast.FindAll({ param($n) $n -is [System.Management.Automation.Language.CommandAst] }, $true)) {
        $elements = $cmd.CommandElements
        if ($elements.Count -eq 0) {
            throw 'CommandAst has no command elements'
        }
        $arguments = [System.Collections.Generic.List[object]]::new()
        for ($i = 1; $i -lt $elements.Count; $i++) {
            $arguments.Add([pscustomobject]@{ span = @((Get-ByteOffset $map $elements[$i].Extent.StartOffset), (Get-ByteOffset $map $elements[$i].Extent.EndOffset)) })
        }
        $redirections = [System.Collections.Generic.List[object]]::new()
        foreach ($redir in $cmd.Redirections) {
            $redirections.Add([int[]]@((Get-ByteOffset $map $redir.Extent.StartOffset), (Get-ByteOffset $map $redir.Extent.EndOffset)))
        }
        $commands.Add([pscustomobject]@{
            start = Get-ByteOffset $map $cmd.Extent.StartOffset
            end = Get-ByteOffset $map $cmd.Extent.EndOffset
            name = @((Get-ByteOffset $map $elements[0].Extent.StartOffset), (Get-ByteOffset $map $elements[0].Extent.EndOffset))
            arguments = $arguments.ToArray()
            redirections = $redirections.ToArray()
        })
    }
    $semantic = [System.Collections.Generic.List[object]]::new()
    foreach ($node in $ast.FindAll({ param($n)
            $n -is [System.Management.Automation.Language.VariableExpressionAst] -or
            $n -is [System.Management.Automation.Language.SubExpressionAst] -or
            $n -is [System.Management.Automation.Language.InvokeMemberExpressionAst] -or
            $n -is [System.Management.Automation.Language.MemberExpressionAst] -or
            $n -is [System.Management.Automation.Language.ParenExpressionAst] -or
            $n -is [System.Management.Automation.Language.ScriptBlockExpressionAst] -or
            $n -is [System.Management.Automation.Language.AssignmentStatementAst]
        }, $true)) {
        if ($node -is [System.Management.Automation.Language.VariableExpressionAst]) { $role = 'variable' }
        elseif ($node -is [System.Management.Automation.Language.SubExpressionAst]) { $role = 'subexpression' }
        elseif ($node -is [System.Management.Automation.Language.InvokeMemberExpressionAst]) { $role = 'method' }
        elseif ($node -is [System.Management.Automation.Language.MemberExpressionAst]) { $role = 'member' }
        elseif ($node -is [System.Management.Automation.Language.ParenExpressionAst]) { $role = 'parentheses' }
        elseif ($node -is [System.Management.Automation.Language.ScriptBlockExpressionAst]) { $role = 'script_block' }
        else { $role = 'assignment' }
        $semantic.Add([pscustomobject]@{
            role = $role
            start = Get-ByteOffset $map $node.Extent.StartOffset
            end = Get-ByteOffset $map $node.Extent.EndOffset
        })
    }
    [pscustomobject]@{
        errors = $diagnostics.ToArray()
        commands = $commands.ToArray()
        semantic = $semantic.ToArray()
    }
}

function Process-RequestLine([string]$Line) {
    $request = $null
    try {
        $request = $Line | ConvertFrom-Json
    } catch {
        Write-ErrorReply $null 'request: malformed JSON'
        return
    }
    $id = Get-ReplyId $request
    if ($null -eq $id) {
        Write-ErrorReply $null 'request: id must be an integer in the u64 range'
        return
    }
    if ($null -eq $request.PSObject.Properties['source']) {
        Write-ErrorReply $id 'request: missing source field'
        return
    }
    $source = $request.source
    if ($source -isnot [string]) {
        Write-ErrorReply $id 'request: source must be a string'
        return
    }
    if ([System.Text.Encoding]::UTF8.GetByteCount($source) -gt 8192) { # keep in sync with MAX_SOURCE_BYTES in src/lib.rs and powershell_differential.options
        Write-ErrorReply $id 'source exceeds the 8192-byte limit'
        return
    }
    try {
        $projection = Get-Projection $source
        Write-Reply ([pscustomobject]@{ type = 'parsed'; id = $id; projection = $projection })
    } catch {
        Write-ErrorReply $id ('projection: ' + $_.Exception.Message)
    }
}

if ("$($PSVersionTable.PSVersion)" -cne '7.6.6') {
    [Console]::Error.WriteLine(("oracle: pinned PowerShell 7.6.6 required, found {0}; refusing to run" -f $PSVersionTable.PSVersion))
    exit 1
}

$framework = 'dotnet/' + [System.Environment]::Version.ToString()
Write-Reply ([pscustomobject]@{ type = 'ready'; id = [long]0; version = "$($PSVersionTable.PSVersion)"; framework = $framework })

$stdin = [Console]::In
while ($true) {
    $line = $null
    try {
        $line = $stdin.ReadLine()
    } catch {
        break
    }
    if ($null -eq $line) { break }
    if ($line.Length -eq 0) { continue }
    try {
        Process-RequestLine $line
    } catch {
        [Console]::Error.WriteLine(('oracle: fatal request handling failure: ' + $_.Exception.Message))
        exit 1
    }
}
