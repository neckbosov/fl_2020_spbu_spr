fn evaluate(s: &str) -> i32 {
    s.split('+').map(|s| s.parse::<i32>().unwrap()).sum()
}

#[cfg(test)]
mod tests {
    use crate::sum::evaluate;

    #[test]
    fn test_evaluate() {
        assert_eq!(evaluate("1+2"), 3);
        assert_eq!(evaluate("2+4+8"), 14);
        assert_eq!(evaluate("11+22"), 33);
        assert_eq!(evaluate("13+42+777"), 832);
        assert_eq!(evaluate("31+24+777"), 832);
    }
}