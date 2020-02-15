fn evaluate(s: &str) -> i32 {
    let mut stack = vec![];
    for c in s.chars() {
        match c {
            d if d.is_digit(10) => stack.push(c.to_digit(10).unwrap() as i32),
            '+' => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                stack.push(x + y);
            }
            '-' => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                stack.push(x - y);
            }
            '*' => {
                let y = stack.pop().unwrap();
                let x = stack.pop().unwrap();
                stack.push(x * y);
            }
            _ => panic!()
        }
    }
    stack.pop().unwrap()
}

#[cfg(test)]
mod tests {
    use crate::polish::evaluate;

    #[test]
    fn test_polish() {
        assert_eq!(evaluate("12+"), 3);
        assert_eq!(evaluate("94-1+"), 6);
        assert_eq!(evaluate("123*+"), 7);
    }
}