#import <Cocoa/Cocoa.h>
#import <objc/runtime.h>

typedef void (*zs_callback_fn)(void *user_data);

@interface ZSControlTarget : NSObject
@property (nonatomic, assign) zs_callback_fn callback;
@property (nonatomic, assign) void *userData;
- (void)onAction:(id)sender;
@end

@implementation ZSControlTarget
- (void)onAction:(id)sender {
    (void)sender;
    if (self.callback) {
        self.callback(self.userData);
    }
}
@end

static const void *kZSTargetAssociationKey = &kZSTargetAssociationKey;

static inline NSRect zs_make_rect(double x, double y, double w, double h) {
    return NSMakeRect((CGFloat)x, (CGFloat)y, (CGFloat)w, (CGFloat)h);
}

void *zs_app_init(void) {
    @autoreleasepool {
        [NSApplication sharedApplication];
        [NSApp setActivationPolicy:NSApplicationActivationPolicyRegular];
        return (__bridge void *)NSApp;
    }
}

void zs_app_activate(void) {
    @autoreleasepool {
        if (NSApp) {
            [NSApp activateIgnoringOtherApps:YES];
        }
    }
}

void zs_app_run(void) {
    @autoreleasepool {
        [NSApp run];
    }
}

void zs_app_pump(void) {
    @autoreleasepool {
        NSEvent *event = [NSApp nextEventMatchingMask:NSEventMaskAny
                                            untilDate:[NSDate distantPast]
                                               inMode:NSDefaultRunLoopMode
                                              dequeue:YES];
        if (event) {
            [NSApp sendEvent:event];
            [NSApp updateWindows];
        }
    }
}

void *zs_window_create(double x, double y, double w, double h, const char *title) {
    @autoreleasepool {
        NSRect frame = zs_make_rect(x, y, w, h);
        NSWindowStyleMask style = NSWindowStyleMaskTitled |
                                  NSWindowStyleMaskClosable |
                                  NSWindowStyleMaskMiniaturizable |
                                  NSWindowStyleMaskResizable;

        NSWindow *window = [[NSWindow alloc] initWithContentRect:frame
                                                       styleMask:style
                                                         backing:NSBackingStoreBuffered
                                                           defer:NO];
        if (title) {
            [window setTitle:[NSString stringWithUTF8String:title]];
        }
        return (__bridge_retained void *)window;
    }
}

void zs_window_show(void *window_ptr) {
    @autoreleasepool {
        if (!window_ptr) return;
        NSWindow *window = (__bridge NSWindow *)window_ptr;
        [window makeKeyAndOrderFront:nil];
    }
}

void *zs_window_content_view(void *window_ptr) {
    @autoreleasepool {
        if (!window_ptr) return NULL;
        NSWindow *window = (__bridge NSWindow *)window_ptr;
        return (__bridge void *)window.contentView;
    }
}

void zs_view_add_subview(void *parent_ptr, void *child_ptr) {
    @autoreleasepool {
        if (!parent_ptr || !child_ptr) return;
        NSView *parent = (__bridge NSView *)parent_ptr;
        NSView *child = (__bridge NSView *)child_ptr;
        [parent addSubview:child];
    }
}

void *zs_button_create(double x, double y, double w, double h, const char *title) {
    @autoreleasepool {
        NSButton *button = [[NSButton alloc] initWithFrame:zs_make_rect(x, y, w, h)];
        [button setButtonType:NSButtonTypeMomentaryPushIn];
        [button setBezelStyle:NSBezelStyleRounded];
        [button setTitle:title ? [NSString stringWithUTF8String:title] : @"Button"];
        return (__bridge_retained void *)button;
    }
}

void *zs_label_create(double x, double y, double w, double h, const char *text) {
    @autoreleasepool {
        NSTextField *label = [[NSTextField alloc] initWithFrame:zs_make_rect(x, y, w, h)];
        [label setStringValue:text ? [NSString stringWithUTF8String:text] : @""];
        [label setEditable:NO];
        [label setBordered:NO];
        [label setDrawsBackground:NO];
        return (__bridge_retained void *)label;
    }
}

void *zs_textfield_create(double x, double y, double w, double h, const char *text) {
    @autoreleasepool {
        NSTextField *field = [[NSTextField alloc] initWithFrame:zs_make_rect(x, y, w, h)];
        [field setStringValue:text ? [NSString stringWithUTF8String:text] : @""];
        return (__bridge_retained void *)field;
    }
}

void *zs_slider_create(double x, double y, double w, double h, double min_val, double max_val, double value) {
    @autoreleasepool {
        NSSlider *slider = [[NSSlider alloc] initWithFrame:zs_make_rect(x, y, w, h)];
        [slider setMinValue:min_val];
        [slider setMaxValue:max_val];
        [slider setDoubleValue:value];
        return (__bridge_retained void *)slider;
    }
}

void *zs_checkbox_create(double x, double y, double w, double h, const char *title, int checked) {
    @autoreleasepool {
        NSButton *checkbox = [[NSButton alloc] initWithFrame:zs_make_rect(x, y, w, h)];
        [checkbox setButtonType:NSButtonTypeSwitch];
        [checkbox setTitle:title ? [NSString stringWithUTF8String:title] : @"Check"];
        [checkbox setState:checked ? NSControlStateValueOn : NSControlStateValueOff];
        return (__bridge_retained void *)checkbox;
    }
}

void zs_control_set_callback(void *control_ptr, void *callback_ptr, void *user_data) {
    @autoreleasepool {
        if (!control_ptr || !callback_ptr) return;
        NSControl *control = (__bridge NSControl *)control_ptr;

        ZSControlTarget *target = [[ZSControlTarget alloc] init];
        target.callback = (zs_callback_fn)callback_ptr;
        target.userData = user_data;

        [control setTarget:target];
        [control setAction:@selector(onAction:)];

        objc_setAssociatedObject(control, kZSTargetAssociationKey, target, OBJC_ASSOCIATION_RETAIN_NONATOMIC);
    }
}
