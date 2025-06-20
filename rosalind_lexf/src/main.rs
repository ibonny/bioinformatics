fn generate_strings(alphabet: &[char], length: usize) -> Vec<String> {
    let mut results = Vec::new();
    let mut current = String::new();

    fn generate_recursive(
        alphabet: &[char],
        length: usize,
        current: &mut String,
        results: &mut Vec<String>,
    ) {
        if current.len() == length {
            results.push(current.clone());
            return;
        }

        for &c in alphabet {
            current.push(c);
            generate_recursive(alphabet, length, current, results);
            current.pop();
        }
    }

    generate_recursive(alphabet, length, &mut current, &mut results);
    results
}

fn main() {
    let input_str = "A B C D E";
    let input_count = 4;

    let alphabet = input_str.replace(" ", "");

    let char_vec: Vec<char> = alphabet.chars().collect();
    let char_slice: &[char] = &char_vec;

    let entries = generate_strings(char_slice, input_count);

    for i in 0..entries.len() {
        println!("{}", entries[i]);
    }
}
