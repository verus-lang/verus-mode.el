use vstd::prelude::*;

mod submodule;

verus! {

fn member1_function() {
    assert(1 == 0 + 1);
}

} // verus!
