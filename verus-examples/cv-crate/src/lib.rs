use vstd::prelude::*;

mod foo;

verus! {

fn foo() {
    assert(1 == 0 + 1);
}

} // verus!
