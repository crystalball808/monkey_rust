use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Statement},
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
        Expression::Prefix(PrefixOperator::Negative, expr) => match eval_expression(*expr)? {
            Object::Integer(integer) => Ok(Object::Integer(-integer)),
            Object::Boolean(_) => Ok(Object::Null),
            Object::Null => Ok(Object::Null),
        },
        Expression::Prefix(PrefixOperator::Not, expr) => match eval_expression(*expr)? {
            Object::Integer(integer) => Ok(Object::Boolean(integer == 0)),
            Object::Boolean(boolean) => Ok(Object::Boolean(!boolean)),
            Object::Null => Ok(Object::Boolean(true)),
        },
        Expression::Infix(infix_operator, left_expr, right_expr, _) => {
            eval_infix(infix_operator, *left_expr, *right_expr)
        }
        Expression::Identifier(ident) => todo!(),
        Expression::If(expression, vec, vec1) => todo!(),
        Expression::Func(vec, vec1) => todo!(),
        Expression::Call(expression, vec) => todo!(),
    }
}

fn eval_infix(
    infix_operator: InfixOperator,
    left_expr: Expression,
    right_expr: Expression,
) -> Result<Object, String> {
    let left_obj = eval_expression(left_expr)?;
    let right_obj = eval_expression(right_expr)?;
    match (infix_operator, left_obj, right_obj) {
        (InfixOperator::Equals, left_obj, right_obj) => Ok(Object::Boolean(left_obj == right_obj)),
        (InfixOperator::NotEquals, left_obj, right_obj) => {
            Ok(Object::Boolean(left_obj != right_obj))
        }
        (_, Object::Null, _) | (_, _, Object::Null) => {
            Err(String::from("Tried to do ariphmetic operation on null"))
        }
        (_, Object::Boolean(_), _) | (_, _, Object::Boolean(_)) => {
            Err(String::from("Tried to do ariphmetic operation on boolean"))
        }
        (InfixOperator::Add, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int + right_int))
        }
        (InfixOperator::Subtract, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int - right_int))
        }
        (InfixOperator::Multiply, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int * right_int))
        }
        (InfixOperator::Divide, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Integer(left_int / right_int))
        }
        (InfixOperator::LessThan, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Boolean(left_int < right_int))
        }
        (InfixOperator::GreaterThan, Object::Integer(left_int), Object::Integer(right_int)) => {
            Ok(Object::Boolean(left_int > right_int))
        }
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
