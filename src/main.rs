use std::{
    convert::{TryFrom, TryInto},
    fmt,
    mem::size_of,
    time::Duration,
};

use rusb::{
    Context, Device, DeviceDescriptor, DeviceHandle, DeviceList, GlobalContext, Language, Result,
    UsbContext,
};

use bytemuck::{cast_ref, Pod, Zeroable};
use log::{debug, error, info};

use iui::controls::{Checkbox, HorizontalBox, Label, Slider, VerticalBox};
use iui::prelude::*;
use std::cell::RefCell;
use std::rc::Rc;

use anyhow::anyhow;

const CC_VIDEO: u8 = 0x0e;
const SC_VIDEOCONTROL: u8 = 1;
// const SC_VIDEOSTREAMING: u8 = 2;
// const SC_VIDEO_INTERFACE_COLLECTION: u8 = 3;
// const VC_INPUT_TERMINAL: u8 = 2;
// const VC_SELECTOR_UNIT: u8 = 4;
const VC_PROCESSING_UNIT: u8 = 5;
const SET_ENTITY_OR_INTERFACE: u8 = 0b00100001;
// const SET_ENDPOINT: u8 = 0b00100010;
const GET_ENTITY_OR_INTERFACE: u8 = 0b10100001;
//const GET_ENDPOINT: u8 = 0b10100010;
const GET_CUR: u8 = 0x81;
const GET_MIN: u8 = 0x82;
const GET_MAX: u8 = 0x83;
// const GET_CUR_ALL: u8 = 0x91;
// const GET_MIN_ALL: u8 = 0x92;
// const GET_MAX_ALL: u8 = 0x93;
const SET_CUR: u8 = 0x01;
// const SET_CUR_ALL: u8 = 0x11;

const PU_BACKLIGHT_COMPENSATION_CONTROL: u8 = 0x01;
const PU_BRIGHTNESS_CONTROL: u8 = 0x02;
const PU_CONTRAST_CONTROL: u8 = 0x03;
const PU_GAIN_CONTROL: u8 = 0x04;
const PU_POWER_LINE_FREQUENCY_CONTROL: u8 = 0x05;
const PU_HUE_CONTROL: u8 = 0x06;
const PU_SATURATION_CONTROL: u8 = 0x07;
const PU_SHARPNESS_CONTROL: u8 = 0x08;
const PU_GAMMA_CONTROL: u8 = 0x09;
const PU_WHITE_BALANCE_TEMPERATURE_CONTROL: u8 = 0x0A;
const PU_WHITE_BALANCE_TEMPERATURE_AUTO_CONTROL: u8 = 0x0B;
const PU_WHITE_BALANCE_COMPONENT_CONTROL: u8 = 0x0C;
const PU_WHITE_BALANCE_COMPONENT_AUTO_CONTROL: u8 = 0x0D;
const PU_DIGITAL_MULTIPLIER_CONTROL: u8 = 0x0E;
const PU_DIGITAL_MULTIPLIER_LIMIT_CONTROL: u8 = 0x0F;
const PU_HUE_AUTO_CONTROL: u8 = 0x10;
const PU_ANALOG_VIDEO_STANDARD_CONTROL: u8 = 0x11;
const PU_ANALOG_LOCK_STATUS_CONTROL: u8 = 0x12;
const PU_CONTRAST_AUTO_CONTROL: u8 = 0x13;

#[derive(Debug, Copy, Clone)]
struct Control {
    mask: u32,
    selector: u8,
    name: &'static str,
    kind: Kind,
}

impl Control {
    pub fn is_supported(&self, flags: u32) -> bool {
        flags & self.mask > 0
    }
}

struct Bof([u8; 16]);

impl From<Bof> for u32 {
    fn from(bof: Bof) -> Self {
        Self::from_le_bytes(bof.0[0..4].try_into().unwrap())
    }
}

impl From<Bof> for i32 {
    fn from(bof: Bof) -> Self {
        Self::from_le_bytes(bof.0[0..4].try_into().unwrap())
    }
}

trait ToString {}

impl ToString for i32 {}

const CONTROLS: [Control; 19] = [
    BRIGHTNESS,
    CONTRAST,
    CONTRAST_AUTO,
    HUE,
    HUE_AUTO,
    SATURATION,
    SHARPNESS,
    GAMMA,
    WHITE_BALANCE_TEMPERATURE,
    WHITE_BALANCE_TEMPERATURE_AUTO,
    WHITE_BALANCE_COMPONENT,
    WHITE_BALANCE_COMPONENT_AUTO,
    BACKLIGHT_COMPENSATION,
    GAIN,
    POWER_LINE_FREQUENCY,
    DIGITAL_MULTIPLIER,
    DIGITAL_MULTIPLIER_LIMIT,
    ANALOG_VIDEO_STANDARD,
    ANALOG_VIDEO_LOCK,
];

#[allow(unused)]
#[derive(Debug, Copy, Clone)]
enum Kind {
    U8,
    U16,
    U32,
    I8,
    I16,
    I32,
    Bool,
    Range(u8, u8),
}

const BRIGHTNESS: Control = Control {
    mask: 0b0000_0001,
    selector: PU_BRIGHTNESS_CONTROL,
    name: "Brightness",
    kind: Kind::I16,
};
const CONTRAST: Control = Control {
    mask: 0b0000_0010,
    selector: PU_CONTRAST_CONTROL,
    name: "Contrast",
    kind: Kind::U16,
};
const HUE: Control = Control {
    mask: 0b0000_0100,
    selector: PU_HUE_CONTROL,
    name: "Hue",
    kind: Kind::I16,
};
const SATURATION: Control = Control {
    mask: 0b0000_1000,
    selector: PU_SATURATION_CONTROL,
    name: "Saturation",
    kind: Kind::U16,
};
const SHARPNESS: Control = Control {
    mask: 0b0001_0000,
    selector: PU_SHARPNESS_CONTROL,
    name: "Sharpness",
    kind: Kind::U16,
};
const GAMMA: Control = Control {
    mask: 0b0010_0000,
    selector: PU_GAMMA_CONTROL,
    name: "Gamma",
    kind: Kind::U16,
};
const WHITE_BALANCE_TEMPERATURE: Control = Control {
    mask: 0b0100_0000,
    selector: PU_WHITE_BALANCE_TEMPERATURE_CONTROL,
    name: "White Balance Temperature",
    kind: Kind::U16,
};
const WHITE_BALANCE_COMPONENT: Control = Control {
    mask: 0b1000_0000,
    selector: PU_WHITE_BALANCE_COMPONENT_CONTROL,
    name: "White Balance Component",
    kind: Kind::U32,
};
const BACKLIGHT_COMPENSATION: Control = Control {
    mask: 0b1_0000_0000,
    selector: PU_BACKLIGHT_COMPENSATION_CONTROL,
    name: "Backlight Compensation",
    kind: Kind::Bool,
};
const GAIN: Control = Control {
    mask: 0b10_0000_0000,
    selector: PU_GAIN_CONTROL,
    name: "Gain",
    kind: Kind::U16,
};
const POWER_LINE_FREQUENCY: Control = Control {
    mask: 0b100_0000_0000,
    selector: PU_POWER_LINE_FREQUENCY_CONTROL,
    name: "Power Line Frequency",
    kind: Kind::Range(0, 4),
};
const HUE_AUTO: Control = Control {
    mask: 0b1000_0000_0000,
    selector: PU_HUE_AUTO_CONTROL,
    name: "Hue Auto",
    kind: Kind::Bool,
};
const WHITE_BALANCE_TEMPERATURE_AUTO: Control = Control {
    mask: 0b1_0000_0000_0000,
    selector: PU_WHITE_BALANCE_TEMPERATURE_AUTO_CONTROL,
    name: "White Balance Temperature Auto",
    kind: Kind::Bool,
};
const WHITE_BALANCE_COMPONENT_AUTO: Control = Control {
    mask: 0b10_0000_0000_0000,
    selector: PU_WHITE_BALANCE_COMPONENT_AUTO_CONTROL,
    name: "White Balance Component Auto",
    kind: Kind::Bool,
};
const DIGITAL_MULTIPLIER: Control = Control {
    mask: 0b100_0000_0000_0000,
    selector: PU_DIGITAL_MULTIPLIER_CONTROL,
    name: "Digital Multiplier",
    kind: Kind::U16,
};
const DIGITAL_MULTIPLIER_LIMIT: Control = Control {
    mask: 0b1000_0000_0000_0000,
    selector: PU_DIGITAL_MULTIPLIER_LIMIT_CONTROL,
    name: "Digital Multiplier Limit",
    kind: Kind::U16,
};
const ANALOG_VIDEO_STANDARD: Control = Control {
    mask: 0b1_0000_0000_0000_0000,
    selector: PU_ANALOG_VIDEO_STANDARD_CONTROL,
    name: "Analog Video Standard",
    kind: Kind::Range(0, 5),
};
const ANALOG_VIDEO_LOCK: Control = Control {
    mask: 0b10_0000_0000_0000_0000,
    selector: PU_ANALOG_LOCK_STATUS_CONTROL,
    name: "Analog Video Lock",
    kind: Kind::U8,
};
const CONTRAST_AUTO: Control = Control {
    mask: 0b100_0000_0000_0000_0000,
    selector: PU_CONTRAST_AUTO_CONTROL,
    name: "Contrast Auto",
    kind: Kind::Bool,
};

#[derive(Debug)]
enum Widget {
    ISlider(i32, i32, i32),
    USlider(u32, u32, u32),
    Stepper(u8, u8, u8),
    Checkbox(bool),
}

#[derive(Debug)]
struct ControlValue {
    control: Control,
    widget: Widget,
}

struct PU<T: UsbContext> {
    device_handle: DeviceHandle<T>,
    controls: Vec<ControlValue>,
    index: u16,
}

impl<T: UsbContext> fmt::Debug for PU<T> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> std::result::Result<(), fmt::Error> {
        let mut debug = fmt.debug_struct("InterfaceDescriptor");

        debug.field("controls", &self.controls);

        debug.finish()
    }
}

impl<T: UsbContext> PU<T> {
    fn new(
        device_handle: DeviceHandle<T>,
        controls: &[&Control],
        interface: u8,
        unit: u8,
    ) -> anyhow::Result<Self> {
        let index = interface as u16 | ((unit as u16) << 8);

        let mut pu = Self {
            device_handle,
            controls: Vec::default(),
            index,
        };

        let mut cvs = Vec::with_capacity(controls.len());
        for control in controls {
            let cur = pu.read_control(control, GET_CUR)?;
            let widget = match control.kind {
                Kind::U8 | Kind::U16 | Kind::U32 => Widget::USlider(
                    pu.read_control(control, GET_MIN)?,
                    cur,
                    pu.read_control(control, GET_MAX)?,
                ),
                Kind::I8 | Kind::I16 | Kind::I32 => Widget::ISlider(
                    pu.read_control(control, GET_CUR)?,
                    cur as i32,
                    pu.read_control(control, GET_MAX)?,
                ),
                Kind::Bool => Widget::Checkbox(cur != 0),
                Kind::Range(min, max) => Widget::Stepper(min, cur as u8, max),
            };
            cvs.push(ControlValue {
                control: **control,
                widget,
            });
        }

        pu.controls = cvs;
        Ok(pu)
    }

    fn read_control<WAT: From<Bof>>(&self, control: &Control, request_type: u8) -> Result<WAT> {
        debug!("reading {}", control.name);

        let timeout = Duration::from_secs(1);
        let value = (control.selector as u16) << 8;
        let mut buf = Bof([0u8; 16]);

        self.device_handle.read_control(
            GET_ENTITY_OR_INTERFACE,
            request_type,
            value,
            self.index,
            &mut buf.0,
            timeout,
        )?;

        Ok(buf.into())
    }

    fn write_control(&self, control: &Control, data: u32) {
        let timeout = Duration::from_secs(1);
        let mut buf = [0u8; 16];

        let data = data.to_le_bytes();
        buf[0] = data[0];
        buf[1] = data[1];
        buf[2] = data[2];
        buf[3] = data[3];

        debug!("setting {} to {:?}", control.name, &buf[0..4]);

        let _ = self
            .device_handle
            .write_control(
                SET_ENTITY_OR_INTERFACE,
                SET_CUR,
                (control.selector as u16) << 8,
                self.index,
                &buf,
                timeout,
            )
            .map_err(|e| {
                error!("update failed: {:?}", e);
            });
    }
}
#[derive(Debug)]
struct Endpoint {
    config: u8,
    iface: u8,
    setting: u8,
    address: u8,
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[allow(non_snake_case)]
#[repr(C, packed)]
struct video_control_header {
    pub bLength: u8,
    pub bDescriptorType: u8,
    pub bDescriptorSubType: u8,
    pub bcdUVC: u16,
    pub wTotalLength: u16,
    pub dwClockFrequency: u32,
    pub bInCollection: u8,
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[allow(non_snake_case)]
#[repr(C)]
struct video_streaming_header {
    pub bLength: u8,
    pub bDescriptorType: u8,
    pub bDescriptorSubType: u8,
    pub bNumFormats: u8,
    pub wTotalLength: u16,
    pub bEndpointAddress: u8,
    pub bmInfo: u8,
    pub bTerminalLink: u8,
    pub bStillCaptureMethod: u8,
    pub bTriggerSupport: u8,
    pub bTriggerUsage: u8,
    pub bControlSize: u8,
    pub bmaControls: u8,
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[allow(non_snake_case)]
#[repr(C, packed)]
struct video_streaming_interface_descriptor {
    pub bLength: u8,
    pub bDescriptorType: u8,
    pub bInterfaceNumber: u8,
    pub bAlternateSetting: u8,
    pub bNumEndpoints: u8,
    pub bInterfaceClass: u8,
    pub bInterfaceSubClass: u8,
    pub bInterfaceProtocol: u8,
    pub iInterface: u8,
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[allow(non_snake_case)]
#[repr(C)]
struct generic_descriptor {
    pub bLength: u8,
    pub bDescriptorType: u8,
    pub bDescriptorSubType: u8,
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[allow(non_snake_case)]
#[repr(C, packed)]
struct selector_unit_descriptor {
    pub bLength: u8,
    pub bDescriptorType: u8,
    pub bDescriptorSubType: u8,
    pub bUnitID: u8,
    pub bNrInPins: u8,
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[allow(non_snake_case)]
#[repr(C, packed)]
struct processing_unit_descriptor {
    pub bLength: u8,
    pub bDescriptorType: u8,
    pub bDescriptorSubType: u8,
    pub bUnitID: u8,
    pub bSourceID: u8,
    pub wMaxMultiplier: u16,
    pub bControlSize: u8,
    pub bmControls: [u8; 3],
    pub iProcessing: u8,
    pub bmVideoStandards: u8,
}

fn main() -> anyhow::Result<()> {
    pretty_env_logger::init();
    let mut context = Context::new()?;
    let device = find_device(&mut context)?;
    let (device_desc, handle) = open_device(&mut context, &device)?;
    let pu = build_pu(device, &device_desc, handle)?;
    gui(pu);
    Ok(())
}

fn gui<T: UsbContext>(pu: PU<T>) {
    let ui = UI::init().unwrap();

    let pu = Rc::new(RefCell::new(pu));

    // The vertical box arranges the inputs within the groups
    let mut input_vbox = VerticalBox::new(&ui);
    input_vbox.set_padded(&ui, true);

    let purc = Rc::clone(&pu);
    let pur = RefCell::borrow(&purc);
    let controls = &pur.controls;
    for cv in controls {
        let purc = pu.clone();
        let (widget, label_text) = match cv.widget {
            Widget::ISlider(min, cur, max) => {
                let mut slider = Slider::new(&ui, min as i64, max as i64);
                slider.set_value(&ui, cur as i64);
                slider.on_changed(&ui, {
                    let state = RefCell::borrow(&purc);
                    move |val| state.write_control(&cv.control, val as u32)
                });
                (iui::controls::Control::from(slider.into()), cv.control.name)
            }
            Widget::USlider(min, cur, max) => {
                let mut slider = Slider::new(&ui, min as i64, max as i64);
                slider.set_value(&ui, cur as i64);
                slider.on_changed(&ui, {
                    let state = RefCell::borrow(&purc);
                    move |val| state.write_control(&cv.control, val as u32)
                });
                (slider.into(), cv.control.name)
            }
            Widget::Stepper(min, cur, max) => {
                let mut slider = Slider::new(&ui, min as i64, max as i64);
                slider.set_value(&ui, cur as i64);
                slider.on_changed(&ui, {
                    let state = RefCell::borrow(&purc);
                    move |val| state.write_control(&cv.control, val as u32)
                });
                (slider.into(), cv.control.name)
            }
            Widget::Checkbox(cur) => {
                let mut checkbox = Checkbox::new(&ui, &cv.control.name);
                checkbox.set_checked(&ui, cur);
                checkbox.on_toggled(&ui, {
                    let state = RefCell::borrow(&purc);
                    move |val| state.write_control(&cv.control, val as u32)
                });

                (checkbox.into(), "")
            }
        };
        input_vbox.append(&ui, widget, LayoutStrategy::Compact);

        if !label_text.is_empty() {
            let label = Label::new(&ui, label_text);
            input_vbox.append(&ui, label.clone(), LayoutStrategy::Compact);
        }
    }

    // This horizontal box will arrange the two groups of controls.
    let mut hbox = HorizontalBox::new(&ui);
    hbox.append(&ui, input_vbox, LayoutStrategy::Stretchy);

    // The window allows all constituent components to be displayed.
    let mut window = Window::new(&ui, "Webcam Control", 300, 150, WindowType::NoMenubar);
    window.set_child(&ui, hbox);
    window.show(&ui);

    let mut event_loop = ui.event_loop();

    event_loop.run(&ui);
}

fn open_device<T: UsbContext>(
    context: &mut T,
    device: &Device<T>,
) -> anyhow::Result<(DeviceDescriptor, DeviceHandle<T>)> {
    if let Ok(handle) = device.open() {
        let device_desc = device.device_descriptor()?;
        let timeout = Duration::from_secs(1);
        let languages = handle.read_languages(timeout)?;

        debug!("Active configuration: {}", handle.active_configuration()?);
        debug!("Languages: {:?}", languages);

        if languages.len() > 0 {
            let language = languages[0];

            info!(
                "Manufacturer: {:?}",
                handle
                    .read_manufacturer_string(language, &device_desc, timeout)
                    .ok()
            );
            info!(
                "Product: {:?}",
                handle
                    .read_product_string(language, &device_desc, timeout)
                    .ok()
            );
            info!(
                "Serial Number: {:?}",
                handle
                    .read_serial_number_string(language, &device_desc, timeout)
                    .ok()
            );
        }
        return Ok((device_desc, handle));
    }

    Err(anyhow!("could not open device"))
}

macro_rules! cast_slice_to_type {
    ($e:expr, $t:ty) => {cast_ref::<_, $t>(  <&[u8; size_of::<$t>()] as TryFrom<&[u8]>>::try_from(
        $e,
    )
    .unwrap())}
}

fn find_device<T: UsbContext>(context: &mut T) -> anyhow::Result<Device<T>> {
    for device in context.devices()?.iter() {
        let device_desc = match device.device_descriptor() {
            Ok(d) => d,
            Err(_) => continue,
        };

        for n in 0..device_desc.num_configurations() {
            let config_desc = match device.config_descriptor(n) {
                Ok(c) => c,
                Err(_) => continue,
            };

            for interface in config_desc.interfaces() {
                for interface_desc in interface.descriptors() {
                    let class = interface_desc.class_code();
                    let subclass = interface_desc.sub_class_code();

                    if class == CC_VIDEO && subclass == SC_VIDEOCONTROL {
                        return Ok(device);
                    }
                }
            }
        }
    }

    Err(anyhow!("could not find a camera device"))
}

fn build_pu<T: UsbContext>(
    device: Device<T>,
    device_desc: &DeviceDescriptor,
    mut handle: DeviceHandle<T>,
) -> anyhow::Result<PU<T>> {
    for n in 0..device_desc.num_configurations() {
        let config_desc = match device.config_descriptor(n) {
            Ok(c) => c,
            Err(_) => continue,
        };

        debug!("extra bytes {:?}", config_desc.extra());

        for interface in config_desc.interfaces() {
            for interface_desc in interface.descriptors() {
                let class = interface_desc.class_code();
                let subclass = interface_desc.sub_class_code();
                let control_mem = interface_desc.extra();
                if control_mem.is_none() {
                    continue;
                }
                let control_mem = control_mem.unwrap();
                if class == CC_VIDEO && subclass == SC_VIDEOCONTROL {
                    let header_slice = &control_mem[0..12];

                    let vch = cast_slice_to_type!(header_slice, video_control_header);
                    debug!("hh {:?}", vch);

                    let mut read = vch.bLength as usize;
                    while read < control_mem.len() {
                        let generic = cast_slice_to_type!(
                            &control_mem[read..read + size_of::<generic_descriptor>()],
                            generic_descriptor
                        );
                        if generic.bDescriptorSubType == VC_PROCESSING_UNIT {
                            let pu = cast_slice_to_type!(
                                &control_mem[read..read + 13],
                                processing_unit_descriptor
                            );
                            let c = &pu.bmControls;

                            handle.claim_interface(interface.number()).unwrap();

                            let interface = interface_desc.interface_number();
                            let unit = pu.bUnitID;

                            let flags = u32::from_le_bytes([c[0], c[1], c[2], 0]);
                            let pu = PU::new(
                                handle,
                                CONTROLS
                                    .iter()
                                    .filter(|&control| control.is_supported(flags))
                                    .collect::<Vec<&Control>>()
                                    .as_slice(),
                                interface,
                                unit,
                            );
                            debug!("found processing unit: {:#?} {:#b}", pu, flags);
                            return pu;
                        }
                        read += generic.bLength as usize;
                    }
                }
            }
        }
    }

    Err(anyhow!("could not build control structure"))
}
