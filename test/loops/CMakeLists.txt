#
# Low-level loop transformation tests
#

# Information for specific test cases
# -----------------------------------
# extract7: with target clause
# fusion7: with target clause
# fusion8: impossible fusion because of assignment
# fusion9: issue 121 (release constraint)
# fusion12: Behavior with other directives (OpenACC)
# fusion10: issue 121 + hoist
# fusion11: test case for ECMWF cloudsc
# fusion13: Behavior with other directives (OpenMP)
# hoist3:  with target clause
# hoist4:  issue 122 (nested in do stmt)
# hoist5:  issue 122 + interchange test
# hoist6:  with fusion group clauses
# hoist7:  with fusion group collapse clauses
# hoist8:  cleanup clause for both OpenACC and OpenMP
# hoist9:  cleanup clause for OpenACC only
# hoist10: cleanup clause for OpenMP only
# ifextract1: if-then block
# ifextract2: if-then-else block
# interchange3: with target clause

# Sepcific flags for some test cases
set(CLAW_FLAGS_fusion10 --config=fusion_first.xml)
set(CLAW_FLAGS_fusion11 --config=dedicated_config.xml)
set(CLAW_FLAGS_fusion13 --directive=openmp)

claw_add_basic_test_set(NAME loops DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
