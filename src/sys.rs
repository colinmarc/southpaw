use bit_vec::BitVec;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

pub(crate) fn set_bit(bv: &mut BitVec, bit: impl Into<usize>) {
    let bit = bit.into();
    bv.grow((bit + 1).saturating_sub(bv.len()), false);
    bv.set(bit, true);
}
