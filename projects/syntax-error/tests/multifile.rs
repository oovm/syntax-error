use syntax_error::{sources, ColorGenerator, FileID, FileSpan, Fmt, Label, Report, ReportKind};

#[test]
fn main() {
    let mut colors = ColorGenerator::new();

    // Generate some colours for each of our elements
    let a = colors.next();
    let b = colors.next();
    let c = colors.next();

    let file_a = FileID::new(1);
    let file_b = FileID::new(2);

    Report::new(ReportKind::Error, file_b, 10)
        .with_code(3)
        .with_message(format!("Cannot add types Nat and Str"))
        .with_label(
            Label::new(file_b.with_range(10..14)).with_message(format!("This is of type {}", "Nat".fg(a))).with_color(a),
        )
        .with_label(
            Label::new(file_b.with_range(17..20)).with_message(format!("This is of type {}", "Str".fg(b))).with_color(b),
        )
        .with_label(
            Label::new(file_b.with_range(15..16))
                .with_message(format!(" {} and {} undergo addition here", "Nat".fg(a), "Str".fg(b)))
                .with_color(c)
                .with_order(10),
        )
        .with_label(
            Label::new(file_a.with_range(4..8))
                .with_message(format!("Original definition of {} is here", "five".fg(a)))
                .with_color(a),
        )
        .with_note(format!("{} is a number and can only be added to other numbers", "Nat".fg(a)))
        .finish()
        .print(sources(vec![(file_a, include_str!("a.tao")), (file_b, include_str!("b.tao"))]))
        .unwrap();
}
