sed -i "s/int8 /int8_t /g;s/int16 /int16_t /g;s/int32 /int32_t /g;s/\/\/\(.*\)/\/\*\1 \*\//;s/	/    /g;s/ReadMacInt16/lduw_p/;s/ReadMacInt32/ldl_p/;s/WriteMacInt16/stw_p/;s/WriteMacInt32/stl_p/" $1
