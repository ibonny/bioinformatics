use std::collections::HashMap;
use std::fs;

fn parse_fasta(input: &str) -> Vec<(String, String)> {
    let mut sequences = Vec::new();
    let mut current_label = String::new();
    let mut current_sequence = String::new();

    for line in input.lines() {
        if line.starts_with('>') {
            if !current_label.is_empty() {
                sequences.push((current_label.clone(), current_sequence.clone()));
                current_sequence.clear();
            }
            current_label = line[1..].to_string();
        } else {
            current_sequence.push_str(line.trim());
        }
    }

    if !current_label.is_empty() {
        sequences.push((current_label, current_sequence));
    }

    sequences
}

fn remove_introns(dna: &str, introns: &[&str]) -> String {
    let mut result = dna.to_string();
    for intron in introns {
        result = result.replace(intron, "");
    }
    result
}

fn dna_to_rna(dna: &str) -> String {
    dna.replace('T', "U")
}

fn rna_to_protein(rna: &str) -> String {
    let codon_table: HashMap<&str, char> = [
        ("UUU", 'F'),
        ("UUC", 'F'),
        ("UUA", 'L'),
        ("UUG", 'L'),
        ("UCU", 'S'),
        ("UCC", 'S'),
        ("UCA", 'S'),
        ("UCG", 'S'),
        ("UAU", 'Y'),
        ("UAC", 'Y'),
        ("UAA", '*'),
        ("UAG", '*'),
        ("UGU", 'C'),
        ("UGC", 'C'),
        ("UGA", '*'),
        ("UGG", 'W'),
        ("CUU", 'L'),
        ("CUC", 'L'),
        ("CUA", 'L'),
        ("CUG", 'L'),
        ("CCU", 'P'),
        ("CCC", 'P'),
        ("CCA", 'P'),
        ("CCG", 'P'),
        ("CAU", 'H'),
        ("CAC", 'H'),
        ("CAA", 'Q'),
        ("CAG", 'Q'),
        ("CGU", 'R'),
        ("CGC", 'R'),
        ("CGA", 'R'),
        ("CGG", 'R'),
        ("AUU", 'I'),
        ("AUC", 'I'),
        ("AUA", 'I'),
        ("AUG", 'M'),
        ("ACU", 'T'),
        ("ACC", 'T'),
        ("ACA", 'T'),
        ("ACG", 'T'),
        ("AAU", 'N'),
        ("AAC", 'N'),
        ("AAA", 'K'),
        ("AAG", 'K'),
        ("AGU", 'S'),
        ("AGC", 'S'),
        ("AGA", 'R'),
        ("AGG", 'R'),
        ("GUU", 'V'),
        ("GUC", 'V'),
        ("GUA", 'V'),
        ("GUG", 'V'),
        ("GCU", 'A'),
        ("GCC", 'A'),
        ("GCA", 'A'),
        ("GCG", 'A'),
        ("GAU", 'D'),
        ("GAC", 'D'),
        ("GAA", 'E'),
        ("GAG", 'E'),
        ("GGU", 'G'),
        ("GGC", 'G'),
        ("GGA", 'G'),
        ("GGG", 'G'),
    ]
    .iter()
    .cloned()
    .collect();

    let mut protein = String::new();
    let codons: Vec<&str> = rna
        .as_bytes()
        .chunks(3)
        .map(|chunk| std::str::from_utf8(chunk).unwrap())
        .collect();

    for codon in codons {
        if codon.len() == 3 {
            let amino_acid = codon_table.get(codon).unwrap();
            if *amino_acid == '*' {
                break;
            }
            protein.push(*amino_acid);
        }
    }

    protein
}

fn main() -> std::io::Result<()> {
    // Read the file contents into a string
    let input = fs::read_to_string("rosalind_splc.fasta")?;

    let sequences = parse_fasta(&input);
    let main_dna = &sequences[0].1;
    let introns: Vec<&str> = sequences[1..].iter().map(|(_, seq)| seq.as_str()).collect();

    let dna_without_introns = remove_introns(main_dna, &introns);
    let rna = dna_to_rna(&dna_without_introns);
    let protein = rna_to_protein(&rna);

    println!("{}", protein);
    Ok(())
}
