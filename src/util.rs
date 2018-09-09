use std::collections::HashMap;
use std::iter::repeat;

use SingleFmtError;

/* ---------- general formatting options ---------- */

pub enum Justification {
    Left(),
    Center(),
    Right(),
    NotSpecified()
}

pub fn apply_common_options(s: &mut String, options: &HashMap<String, String>)
    -> Result<(), SingleFmtError>
{
    apply_width(s, options)?;
    Ok(())
}

pub fn apply_width(s: &mut String, options: &HashMap<String, String>)
    -> Result<(), SingleFmtError>
{
    if let Some(width_str) = options.get("width") {
        if let Ok(width) = width_str.parse::<usize>() {
            let len = s.chars().count();
            if len > width {
                return Ok(());
            }
            let delta = width - len;
            match get_justification(options)? {
                Justification::Left() | Justification::NotSpecified() =>  {
                    let padding: String = repeat(' ').take(delta).collect();
                    s.push_str(&padding);
                }
                Justification::Center() => {
                    let left = delta / 2;
                    let right = delta - left;
                    let leftstr: String = repeat(' ').take(left).collect();
                    let rightstr: String = repeat(' ').take(right).collect();
                    *s = leftstr + s + &rightstr;
                }
                Justification::Right() => {
                    let padding: String = repeat(' ').take(delta).collect();
                    *s = padding + s;
                }
            }
        } else {
            return Err(SingleFmtError::InvalidOptionValue("width".to_string(),
                width_str.to_string()));
        }
    }
    Ok(())
}

/* ---------- helpers ---------- */

pub fn get_justification(options: &HashMap<String, String>) 
    -> Result<Justification, SingleFmtError>
{
    if let Some(s) = options.get("just") {
        match s.as_str() {
            "left" => Ok(Justification::Left()),
            "right" => Ok(Justification::Right()),
            "center" => Ok(Justification::Center()),
            _ => Err(SingleFmtError::InvalidOptionValue("just".to_string(),
                s.to_string()))
        }
    } else {
        Ok(Justification::NotSpecified())
    }
}
