use std::ops::Deref;

use crate::{
    ast::{Expression, PrefixOperator, Statement},
    object::Object,
};

pub fn eval_statements(statements: Vec<Statement>) -> Result<Object, String> {
    let mut result = Object::Null;

    for statement in statements {
        result = eval_statement(statement)?;
    }

    Ok(result)
}
fn eval_expression(expr: Expression) -> Result<Object, String> {
    match expr {
        Expression::IntLiteral(integer) => Ok(Object::Integer(integer)),
        Expression::Boolean(boolean) => Ok(Object::Boolean(boolean)),
        Expression::Identifier(_) => todo!(),
        Expression::Prefix(PrefixOperator::Negative, _) => todo!(),
        Expression::Prefix(PrefixOperator::Not, expr) => match eval_expression(*expr)? {
            Object::Integer(integer) => Ok(Object::Boolean(integer == 0)),
            Object::Boolean(boolean) => Ok(Object::Boolean(!boolean)),
            Object::Null => Ok(Object::Boolean(true)),
        },
        Expression::Infix(infix_operator, expression, expression1, _) => todo!(),
        Expression::If(expression, vec, vec1) => todo!(),
        Expression::Func(vec, vec1) => todo!(),
        Expression::Call(expression, vec) => todo!(),
    }
}

fn eval_statement(statement: Statement) -> Result<Object, String> {
    match statement {
        Statement::Let(_, expression) => todo!(),
        Statement::Return(expression) => todo!(),
        Statement::Expression(expr) => eval_expression(expr),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{Lexer, parser::Parser};

    #[test]
    fn integer() {
        let lexer = Lexer::new("10;");
        let parser = Parser::new(lexer);
        let parsed_ast = parser
            .parse_program()
            .expect("Should be parsed successfully");
        let result = eval_statements(parsed_ast.statements).expect("Should evaluate");

        assert_eq!(result, Object::Integer(10));
    }
}
