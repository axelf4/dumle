//! Utilities for HTML escaping.
//!
//! See: [Using character escapes in markup and CSS][Using character escapes in markup and CSS].
//!
//! [Using character escapes in markup and CSS]: https://www.w3.org/International/questions/qa-escapes#use

use std::{fmt, str::Chars};

/// HTML entity encodes the given string.
///
/// # Examples
///
/// ```
/// use dumle::html_escape::html_escape;
/// assert_eq!(html_escape(r#""bread" & "butter""#).to_string(), "&quot;bread&quot; &amp; &quot;butter&quot;");
/// ```
pub fn html_escape<'a>(s: &'a str) -> HtmlEscape<'a> {
    HtmlEscape { chars: s.chars() }
}

/// HTML escaper.
#[derive(Debug)]
pub struct HtmlEscape2<W>(pub W);

impl<W: fmt::Write> fmt::Write for HtmlEscape2<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        fn should_escape(b: u8) -> bool {
            (b | 0x4) == b'&' || (b | 0x2) == b'>'
        }

        s.chars()
            .try_for_each(|c| match (c.is_ascii() && should_escape(c as u8), c) {
                (true, '&') => self.0.write_str("&amp"),
                (true, '<') => self.0.write_str("&lt"),
                (true, '>') => self.0.write_str("&gt"),
                (true, '"') => self.0.write_str("&quot"),
                (true, '\'') => self.0.write_str("&apos"),
                _ => self.0.write_char(c),
            })
    }
}

/// The return type of [`html_escape`].
#[derive(Clone, Debug)]
pub struct HtmlEscape<'a> {
    chars: Chars<'a>,
}

impl<'a> Iterator for HtmlEscape<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        let str_slice = self.chars.as_str();
        self.chars.next().map(|c| match c {
            '&' => "&amp;",
            '<' => "&lt;",
            '>' => "&gt;",
            '"' => "&quot;",
            '\'' => "&apos;",
            _ => &str_slice[0..c.len_utf8()],
        })
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.chars.size_hint()
    }
}

impl<'a> fmt::Display for HtmlEscape<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for c in (*self).clone() {
            f.write_str(c)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_xss() {
        assert_eq!(
            html_escape("<SCRIPT SRC=http://xss.rocks/xss.js></SCRIPT>").to_string(),
            "&lt;SCRIPT SRC=http://xss.rocks/xss.js&gt;&lt;/SCRIPT&gt;"
        );
    }
}
