rm -r /target/*

for i in\
    "srfi-27/srfi/27"\
    "r6rs-lists-0.0.1/r6rs/lists"\
    "r6rs-sorting-0.0.1/r6rs/sorting"\
    "r6rs-enums-0.0.1/r6rs/enums"\
    "srfi-126/r6rs/hashtables"\
    "srfi-126/srfi/126"\
    "srfi-128/srfi/128"\
    "srfi-125/srfi/125"\
    "srfi-143/srfi-143/srfi-143"\
    "srfi-145/srfi/145"\
    "srfi-158/srfi-158"\
    "srfi-151/srfi-151/srfi-151"\
    "srfi-146/srfi/146"\
    "srfi-146/srfi/146/hash"
do
    CLASSPATH=target kawa -d target -C "dependencies/$i.sld"
done

cd "/test/srfi-225"
CLASSPATH=/target kawa -d /target -C "srfi/225/core.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/default-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/indexes.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/alist-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/srfi-69-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/srfi-125-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/srfi-126-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/srfi-146-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225/srfi-146-hash-impl.sld"
CLASSPATH=/target kawa -d /target -C "srfi/225.sld"
CLASSPATH=/target kawa "srfi-225-test.scm"
