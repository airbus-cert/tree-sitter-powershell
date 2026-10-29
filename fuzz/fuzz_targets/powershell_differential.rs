#![no_main]

use libfuzzer_sys::{fuzz_target, Corpus};

fuzz_target!(|input: &[u8]| -> Corpus {
    tree_sitter_powershell_fuzz::fuzz(input)
});
