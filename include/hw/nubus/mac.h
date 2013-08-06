#include "hw/nubus/nubus.h"

#define TYPE_MAC_NUBUS_BRIDGE "mac-nubus-bridge"
#define MAC_NUBUS_BRIDGE(obj) OBJECT_CHECK(MacNubusState, (obj), \
                                           TYPE_MAC_NUBUS_BRIDGE)

typedef struct MacNubusState {
    SysBusDevice sysbus_dev;
    NubusBus *bus;
} MacNubusState;

NubusBus *nubus_mac_new(hwaddr super_slot_base, hwaddr slot_base);
