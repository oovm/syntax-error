use syntax_error::{Report, ReportKind, Label, Source};

#[test]
fn main() {
    Report::new(ReportKind::Blame, (), 34)
        .with_message("Incompatible types")
        .with_code(12)
        .with_label(Label::new(32..33).with_message("This is of type Nat"))
        .with_label(Label::new(42..45).with_message("This is of type Str"))
        .finish()
        .print(Source::from(include_str!("sample.tao")))
        .unwrap();
}
