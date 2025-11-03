import XCTest
import SwiftTreeSitter
import TreeSitterPowershell

final class TreeSitterPowershellTests: XCTestCase {
    func testCanLoadGrammar() throws {
        let parser = Parser()
        let language = Language(language: tree_sitter_powershell())
        XCTAssertNoThrow(try parser.setLanguage(language),
                         "Error loading Powershell grammar")
    }
}
