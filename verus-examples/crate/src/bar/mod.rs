use vstd::prelude::*;

mod baz;

verus! {

    proof fn bar() {
        assert(true);
    }

}
