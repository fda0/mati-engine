FixedArray :: struct($T: Type, $CAPACITY: s64)
{
  buf: [CAPACITY] T = ---;
  count: s64;

  ELEMENT_SIZE :: size_of(T);
  DATA_SIZE :: CAPACITY * ELEMENT_SIZE;
}

to_view :: (c: FixedArray) -> [] c.T
{
  return ([] c.T).{data = c.buf.data, count = c.count};
}

to_string :: (c: FixedArray(u8, $CAP)) -> string
{
  return string.{data = c.buf.data, count = c.count};
}

operator [] :: (c: FixedArray, i: s64) -> c.T
{
  assert(i < c.count);
  return c.buf[i];
}

operator []= :: inline (c: *FixedArray, i: int, value: c.T)
{
  assert(i < c.count);
  c.buf[i] = value;
}

for_expansion :: (c: *FixedArray, body: Code, flags: For_Flags) #expand
{
  #assert(flags & ~.POINTER == 0);

  `it_index := -1;
  while true
  {
    it_index += 1;
    if it_index >= c.count {
      break;
    }

    #if flags & .POINTER {
      `it := *c.buf[it_index];
    } else {
      `it := c.buf[it_index];
    }

    #insert (remove = {
      unordered_remove_by_index(c, it_index);
      it_index -= 1;
    }) body;
  }
}

unordered_remove_by_index :: inline (c: *FixedArray, index: s64) {
    assert(index >= 0);
    assert(index < c.count);

    if c.count > 1 {
      c.buf[index] = c.buf[c.count - 1];
    }
    c.count -= 1;
}

size_bytes :: (c: FixedArray) -> s64 {
  return c.count * c.ELEMENT_SIZE;
}

space_left :: (c: FixedArray) -> s64 {
  return c.CAPACITY - c.count;
}

has_space :: (c: FixedArray) -> bool {
  return space_left(c) > 0;
}

is_full :: (c: FixedArray) -> bool {
  return space_left(c) == 0;
}

reset :: (c: *FixedArray) {
  c.count = 0;
}

add :: (c: *FixedArray, item: c.T) -> bool
{
  added := false;
  if c.count < c.CAPACITY
  {
    added = true;
    c.buf[c.count] = item;
    c.count += 1;
  }
  return added;
}

add :: (c: *FixedArray, $initialize := true, $truncate := false) -> *c.T, bool
{
  alloc := true;

  #if truncate
  {
    alloc = c.count < c.CAPACITY;
  }
  else
  {
    assert(c.count < c.CAPACITY);
  }

  if alloc
  {
    result := *c.buf[c.count];
    c.count += 1;
    #if initialize Initialize(result);
    return result, true;
  }

  return null, false;
}

add_many :: (c: *FixedArray, alloc_count: s64, $initialize := true, $truncate := false) -> allocated_slice: [] c.T, bool
{
  fully_written := true;
  space_left := c.CAPACITY - c.count;

  #if truncate
  {
    if alloc_count > space_left
    {
      alloc_count = space_left;
      fully_written = false;
    }
  }
  else
  {
    assert(alloc_count <= space_left);
  }

  if alloc_count <= 0 then return .[], false;

  res: [] c.T;
  res.count = alloc_count;
  res.data = *c.buf[c.count];

  #if initialize
  {
    ini :: initializer_of(c.T);
    #if ini
    {
      for *res
      {
        inline ini(it);
      }
    }
    else
    {
      memset(res.data, 0, res.count * size_of(c.T));
    }
  }

  c.count += alloc_count;
  return res, fully_written;
}

push_bytes :: (c: *FixedArray(u8, $CAP), data: *void, size: s64, $truncate := false) -> bool
{
  slice, fully_written := add_many(c, size, initialize=false, truncate=truncate);
  memcpy(slice.data, data, slice.count);
  return fully_written;
}
