use std::iter::Peekable;

use proc_macro::{
    token_stream::IntoIter, Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream,
    TokenTree,
};

#[proc_macro_attribute]
pub fn concat_cfg(meta: TokenStream, ts: TokenStream) -> TokenStream {
    let predicates = match separated(meta, ',', CfgPredicate::try_from_token_stream, true) {
        Ok(predicates) => predicates,
        Err(err) => return err.into_compile_error().into_iter().chain(ts).collect(),
    };

    [
        Punct::new('#', Spacing::Joint).into(),
        Group::new(
            Delimiter::Bracket,
            [
                TokenTree::Ident(Ident::new("cfg", Span::call_site())),
                Group::new(
                    Delimiter::Parenthesis,
                    predicates
                        .into_iter()
                        .flat_map(|cfg| {
                            [
                                cfg.into_cfg(),
                                TokenTree::Punct(Punct::new(',', Spacing::Alone)).into(),
                            ]
                        })
                        .collect(),
                )
                .into(),
            ]
            .into_iter()
            .collect(),
        )
        .into(),
    ]
    .into_iter()
    .chain(ts)
    .collect()
}

fn is_comma(tt: &TokenTree) -> bool {
    matches!(tt, TokenTree::Punct(punct) if punct.as_char() == ',')
}

#[derive(Debug)]
struct CfgPredicateLiteral {
    value: String,
}

impl CfgPredicateLiteral {
    fn try_from_token_stream(tt: TokenStream) -> Result<Vec<Self>, CfgParseError> {
        Ok(separated(
            tt,
            '+',
            |ts| {
                let mut tts = ts.into_iter().collect::<Vec<_>>();

                assert!(!tts.len() != 1);

                match tts.pop().unwrap() {
                    TokenTree::Group(group) => {
                        if group.delimiter() == Delimiter::None {
                            CfgPredicateLiteral::try_from_token_stream(group.stream())
                            // token_stream_to_token_tree(group.stream())?.
                            // try_into()
                        } else {
                            todo!("AAAHHHH")
                        }
                    }
                    TokenTree::Ident(_) => todo!("expected literal, found ident"),
                    TokenTree::Punct(_) => todo!("expected literal, found punct"),
                    TokenTree::Literal(literal) => {
                        let string = literal.to_string();

                        let str = if (
                            // string literal or byte string literal
                            (string.starts_with('"') || string.starts_with("b\""))
                                && string.ends_with('"')
                        ) || (
                            // char literal or byte literal
                            (string.starts_with('\'') || string.starts_with("b'"))
                                && string.ends_with('\'')
                        ) {
                            &string[1..string.len() - 1]
                        } else {
                            &string[..]
                        };

                        Ok(vec![CfgPredicateLiteral {
                            value: str.to_string(),
                        }])
                    }
                }
            },
            false,
        )?
        .into_iter()
        .flatten()
        .collect())
    }
}

#[derive(Debug)]
enum CfgPredicate {
    CfgOption {
        ident: Ident,
        values: Vec<CfgPredicateLiteral>,
    },
    All(Ident, Vec<CfgPredicate>),
    Any(Ident, Vec<CfgPredicate>),
    Not(Ident, Box<CfgPredicate>),
}

impl CfgPredicate {
    fn try_from_token_stream(ts: TokenStream) -> Result<Self, CfgParseError> {
        let mut ts = ts.into_iter().peekable();

        match ts.next().ok_or(CfgParseError::EndOfStream)? {
            TokenTree::Group(_) => unreachable!(),
            TokenTree::Ident(ident) => match &*ident.to_string() {
                "all" => parse_predicate_group(ts, ident.clone(), |tt| {
                    separated(tt, ',', CfgPredicate::try_from_token_stream, true)
                        .map(|preds| Self::All(ident.clone(), preds))
                }),
                "any" => parse_predicate_group(ts, ident.clone(), |tt| {
                    separated(tt, ',', CfgPredicate::try_from_token_stream, true)
                        .map(|preds| Self::Any(ident.clone(), preds))
                }),
                "not" => parse_predicate_group(ts, ident.clone(), |tt| {
                    CfgPredicate::try_from_token_stream(tt)
                        .map(Box::new)
                        .map(|pred| Self::Not(ident.clone(), pred))
                }),
                _ => match ts.next() {
                    Some(TokenTree::Punct(punct)) => parse_as_ident(&punct, &mut ts, ident),
                    Some(TokenTree::Group(group)) => Err(CfgParseError::UnexpectedToken(
                        group.span_open(),
                        vec![',', '=', '('],
                    )),
                    Some(TokenTree::Ident(ident)) => Err(CfgParseError::UnexpectedToken(
                        ident.span(),
                        vec![',', '=', '('],
                    )),
                    Some(TokenTree::Literal(literal)) => Err(CfgParseError::UnexpectedToken(
                        literal.span(),
                        vec![',', '=', '('],
                    )),
                    None => Ok(CfgPredicate::CfgOption {
                        ident,
                        values: vec![],
                    }),
                },
            },
            TokenTree::Punct(_) => todo!(),
            TokenTree::Literal(_) => todo!(),
        }
    }

    fn into_cfg(self) -> TokenStream {
        match self {
            CfgPredicate::CfgOption { ident, values } => [
                ident.into(),
                TokenTree::Punct(Punct::new('=', Spacing::Alone)),
                TokenTree::Group(Group::new(
                    Delimiter::None,
                    TokenTree::Literal(Literal::string(
                        &values.into_iter().map(|lit| lit.value).collect::<String>(),
                    ))
                    .into(),
                )),
            ]
            .into_iter()
            .collect(),
            CfgPredicate::All(ident, preds) | CfgPredicate::Any(ident, preds) => [
                ident.into(),
                TokenTree::Group(Group::new(
                    Delimiter::Parenthesis,
                    preds
                        .into_iter()
                        .flat_map(|cfg| {
                            [
                                cfg.into_cfg(),
                                TokenTree::Punct(Punct::new(',', Spacing::Alone)).into(),
                            ]
                        })
                        .collect(),
                )),
            ]
            .into_iter()
            .collect(),
            CfgPredicate::Not(ident, pred) => [
                ident.into(),
                TokenTree::Group(Group::new(Delimiter::Parenthesis, pred.into_cfg())),
            ]
            .into_iter()
            .collect(),
        }
    }
}

fn separated<T>(
    ts: TokenStream,
    punct: char,
    parse_fn: fn(TokenStream) -> Result<T, CfgParseError>,
    allow_trailing: bool,
) -> Result<Vec<T>, CfgParseError> {
    let tts = ts.into_iter().collect::<Vec<_>>();

    let is_punct = |tt: &TokenTree| matches!(tt, TokenTree::Punct(p) if p.as_char() == punct);

    let mut output = vec![];

    if let Some(last) = tts.last() {
        if is_punct(last) && !allow_trailing {
            return Err(CfgParseError::UnexpectedToken(last.span(), vec![punct]));
        }
    }

    for ts in tts.split(is_punct) {
        output.push(parse_fn(ts.iter().cloned().collect())?);
    }

    Ok(output)
}

fn parse_predicate_group(
    mut ts: Peekable<IntoIter>,
    ident: Ident,
    ctor: impl Fn(TokenStream) -> Result<CfgPredicate, CfgParseError>,
) -> Result<CfgPredicate, CfgParseError> {
    match ts.next() {
        Some(TokenTree::Group(group)) => {
            if group.delimiter() == Delimiter::Parenthesis {
                ctor(group.stream())
            } else {
                Err(CfgParseError::UnexpectedToken(group.span_open(), vec!['(']))
            }
        }
        Some(TokenTree::Punct(punct)) => parse_as_ident(&punct, &mut ts, ident),
        None => Ok(CfgPredicate::CfgOption {
            ident,
            values: vec![],
        }),
        _ => {
            todo!("proper error for this case")
        }
    }
}

fn parse_as_ident(
    punct: &proc_macro::Punct,
    ts: &mut Peekable<IntoIter>,
    ident: Ident,
) -> Result<CfgPredicate, CfgParseError> {
    match (punct.as_char(), punct.spacing()) {
        ('=', Spacing::Alone) => {
            let values = CfgPredicateLiteral::try_from_token_stream(
                ts.by_ref().take_while(|tt| !is_comma(tt)).collect(),
            )?;

            Ok(CfgPredicate::CfgOption { ident, values })
        }
        (',', Spacing::Alone) => Ok(CfgPredicate::CfgOption {
            ident,
            values: vec![],
        }),
        (_, _) => {
            todo!("proper error for this case")
        }
    }
}

enum CfgParseError {
    EndOfStream,
    UnexpectedToken(Span, Vec<char>),
}

impl CfgParseError {
    fn into_compile_error(self) -> TokenStream {
        let span = match self {
            CfgParseError::EndOfStream => Span::call_site(),
            CfgParseError::UnexpectedToken(span, _) => span,
        };

        let msg = &match self {
            CfgParseError::EndOfStream => "Unexpected end of input".to_string(),
            CfgParseError::UnexpectedToken(_, expected) => {
                "Unexpected token, expected one of ".to_string()
                    + &expected
                        .into_iter()
                        .map(|c| ['`', c, '`'].into_iter().collect())
                        .collect::<Vec<String>>()
                        .join(", ")
            }
        };
        [
            TokenTree::Ident(Ident::new("compile_error", span)),
            Punct::new('!', Spacing::Alone).into(),
            Group::new(
                Delimiter::Brace,
                [TokenTree::Literal(Literal::string(msg))]
                    .into_iter()
                    .map(|mut tt| {
                        tt.set_span(span);
                        tt
                    })
                    .collect(),
            )
            .into(),
        ]
        .into_iter()
        .map(|mut tt| {
            tt.set_span(span);
            tt
        })
        .collect()
    }
}
