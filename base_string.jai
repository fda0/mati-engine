MatchFlags :: enum_flags u32
{
  FindLast;
  RightSideSloppy;
  CaseInsensitive;
  SlashInsensitive;

  FindAddHaystackCountOnFail;
  FindAddNeedleCountOnSuccess;
}

StrFirstLast :: (first: *u8, last: *u8) -> string
{
  res: string;
  res.data = first;
  res.count = max(cast(s64)last - cast(s64)first, 0);
  return res;
}
SubstrMinMax :: (str: string, sub_min: s64, sub_max: s64) -> string
{
  sub_max = clamp(sub_max, 0, str.count);
  sub_min = clamp(sub_min, 0, str.count);
  if (sub_min > sub_max) swap(*sub_min, *sub_max);

  res: string;
  res.count = sub_max - sub_min;
  res.data = str.data + sub_min;
  return res;
}
Substr :: (str: string, offset: s64, count: s64) -> string
{
  offset = clamp(offset, 0, str.count);
  count = min(count, str.count - offset);
  count = max(count, 0);

  res: string;
  res.count = count;
  res.data = str.data + offset;
  return res;
}
Skip :: (str: string, distance: s64) -> string
{
  distance = clamp(distance, 0, str.count);
  res := str;
  res.data += distance;
  res.count -= distance;
  return res;
}
Chop :: (str: string, distance_from_end: s64) -> string
{
  distance_from_end = clamp(distance_from_end, 0, str.count);
  res := str;
  res.count -= distance_from_end;
  return res;
}
Prefix :: (str: string, count: s64) -> string
{
  count = clamp(count, 0, str.count);

  res := str;
  res.count = count;
  return res;
}
Suffix :: (str: string, count: s64) -> string
{
  count = clamp(count, 0, str.count);
  distance := str.count - count;

  res := str;
  res.data += distance;
  res.count = count;
  return res;
}

ByteStr :: (b: *u8) -> string { return string.{data = b, count = 1}; }

StartsWith :: (haystack: string, needle: string, flags: MatchFlags = 0) -> bool
{
  h := Prefix(haystack, needle.count);
  return Match(h, needle, flags);
}
EndsWith :: (haystack: string, needle: string, flags: MatchFlags = 0) -> bool
{
  h := Suffix(haystack, needle.count);
  return Match(h, needle, flags);
}

ToArray :: (str: string) -> [] u8
{
  return [] u8.{data = str.data, count = str.count};
}

IsWhite :: (b: u8) -> bool
{
  res := false;
  res |= b == #char " ";
  res |= b == #char "\t";
  res |= b == #char "\n";
  res |= b == #char "\r";
  return res;
}

IsDigit :: (b: u8) -> bool {
  return b >= #char "0" && b <= #char "9";
}
IsAlphaLower :: (b: u8) -> bool {
  return b >= #char "a" && b <= #char "z";
}
IsAlphaUpper :: (b: u8) -> bool {
  return b >= #char "A" && b <= #char "Z";
}
IsAlpha :: (b: u8) -> bool {
  return IsAlphaLower(b) || IsAlphaUpper(b);
}
IsAlphaNumeric :: (b: u8) -> bool {
  return IsAlpha(b) || IsDigit(b);
}

ToLower :: (b: u8) -> u8
{
  if b >= #char "A" && b <= #char "Z"
  {
    b += (#char "a" - #char "A").(u8);
  }
  return b;
}
ToUpper :: (b: u8) -> u8
{
  if b >= #char "a" && b <= #char "z"
  {
    b -= (#char "a" - #char "A").(u8);
  }
  return b;
}
ToForwardSlash :: (b: u8) -> u8
{
  return ifx b == #char "\\" then #char "/" else b;
}

Match :: (a: string, b: string, flags: MatchFlags = 0) -> bool
{
  result := false;
  if (a.count == b.count || flags & .RightSideSloppy)
  {
    result = true;

    i := 0;
    while true
    {
      if !(i < a.count && i < b.count) break;
      defer i += 1;

      match := (a.data[i] == b.data[i]);

      if (flags & .CaseInsensitive)
      {
        match |= ToLower(a.data[i]) == ToLower(b.data[i]);
      }
      if (flags & .SlashInsensitive)
      {
        match |= ToForwardSlash(a.data[i]) == ToForwardSlash(b.data[i]);
      }

      if !match
      {
        result = false;
        break;
      }
    }
  }
  return result;
}

Find :: (haystack: string, needle: string, flags: MatchFlags = 0) -> s64, bool
{
  flags |= .RightSideSloppy;

  result := 0;
  found := false;

  if flags & .FindLast
  {
    i := haystack.count - needle.count;
    while i >= 0
    {
      h := Skip(haystack, i);
      found = Match(h, needle, flags);
      if found
      {
        result = i;
        break;
      }
      i -= 1;
    }
  }
  else
  {
    i := 0;
    while i + needle.count <= haystack.count
    {
      h := Skip(haystack, i);
      found = Match(h, needle, flags);
      if found
      {
        result = i;
        break;
      }
      i += 1;
    }
  }

  if !found && (flags & .FindAddHaystackCountOnFail) then result = haystack.count;
  if found && (flags & .FindAddNeedleCountOnSuccess) then result += needle.count;
  return result, found;
}

Contains :: (haystack: string, needle: string, flags: MatchFlags = 0) -> bool
{
  idx, found := Find(haystack, needle, flags);
  return found;
}

Trim :: (str: string) -> string
{
  res := str;
  while res.count > 0 && IsWhite(res.data[0]) {
    res.data += 1;
    res.count -= 1;
  }
  while res.count > 0 && IsWhite(res.data[res.count - 1]) {
    res.count -= 1;
  }
  return res;
}

ConsumeFront :: (str: *string, prefix: string, flags: MatchFlags = 0) -> bool
{
  if StartsWith(str.*, prefix, flags)
  {
    str.* = Skip(str.*, prefix.count);
    return true;
  }
  return false;
}
ConsumeBack :: (str: *string, suffix: string, flags: MatchFlags = 0) -> bool
{
  if EndsWith(str.*, suffix, flags)
  {
    str.* = Chop(str.*, suffix.count);
    return true;
  }
  return false;
}

DecodeUTF8 :: (str: string) -> codepoint:u32, byte_length:s64
{
  codepoint: u32 = ~cast(u32)0;
  byte_length: s64 = 1;
  if str.count <= 0 then return codepoint, byte_length;

  BITMASK_3 :: 0b111;
  BITMASK_4 :: 0b1111;
  BITMASK_5 :: 0b111_11;
  BITMASK_6 :: 0b111_111;
  utf8_class: [32] u8 = .[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,2,2,2,2,3,3,4,5];

  byte := str[0];
  byte_class := utf8_class[byte >> 3];

  if byte_class <= str.count
  {
    if byte_class == {
      case 1;
      codepoint = byte;
      byte_length = 1;

      case 2;
      cont_byte := str[1];
      if utf8_class[cont_byte >> 3] == 0
      {
        codepoint  = (byte      & BITMASK_5).(u32) << 6;
        codepoint |= (cont_byte & BITMASK_6).(u32);
        byte_length = 2;
      }

      case 3;
      cont_byte: [2] u8 = .[str[1], str[2]];
      if utf8_class[cont_byte[0] >> 3] == 0 &&
         utf8_class[cont_byte[1] >> 3] == 0
      {
        codepoint  = (byte         & BITMASK_4).(u32) << 12;
        codepoint |= (cont_byte[0] & BITMASK_6).(u32) << 6;
        codepoint |= (cont_byte[1] & BITMASK_6).(u32);
        byte_length = 3;
      }

      case 4;
      cont_byte: [3] u8 = .[str[1], str[2], str[3]];
      if utf8_class[cont_byte[0] >> 3] == 0 &&
         utf8_class[cont_byte[1] >> 3] == 0 &&
         utf8_class[cont_byte[2] >> 3] == 0
      {
        codepoint  = (byte         & BITMASK_3).(u32) << 18;
        codepoint |= (cont_byte[0] & BITMASK_6).(u32) << 12;
        codepoint |= (cont_byte[1] & BITMASK_6).(u32) <<  6;
        codepoint |= (cont_byte[2] & BITMASK_6).(u32);
        byte_length = 4;
      }
    }
  }

  return codepoint, byte_length;
}

#scope_file
#import "Basic"; // for clamp
