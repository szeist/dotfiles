--[[
Clock Rings by Linux Mint (2011) reEdited by despot77

This script draws percentage meters as rings, and also draws clock hands if you want! It is fully customisable; all options are described in the script. This script is based off a combination of my clock.lua script and my rings.lua script.

IMPORTANT: if you are using the 'cpu' function, it will cause a segmentation fault if it tries to draw a ring straight away. The if statement on line 145 uses a delay to make sure that this doesn't happen. It calculates the length of the delay by the number of updates since Conky started. Generally, a value of 5s is long enough, so if you update Conky every 1s, use update_num>5 in that if statement (the default). If you only update Conky every 2s, you should change it to update_num>3; conversely if you update Conky every 0.5s, you should use update_num>10. ALSO, if you change your Conky, is it best to use "killall conky; conky" to update it, otherwise the update_num will not be reset and you will get an error.

To call this script in Conky, use the following (assuming that you save this script to ~/scripts/rings.lua):
    lua_load ~/scripts/clock_rings.lua
    lua_draw_hook_pre clock_rings

Changelog:
+ v1.0 -- Original release (30.09.2009)
   v1.1p -- Jpope edit londonali1010 (05.10.2009)
*v 2011mint -- reEdit despot77 (18.02.2011)
]]

settings_table = {
    {
        name='cpu',
        arg='cpu0',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=35, y=25,
        radius=20,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='cpu',
        arg='cpu1',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=85, y=25,
        radius=20,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='cpu',
        arg='cpu2',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=135, y=25,
        radius=20,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='cpu',
        arg='cpu3',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=185, y=25,
        radius=20,
        thickness=5,
        start_angle=-90,
        end_angle=180
    },
    {
        name='loadavg',
        arg='1',
        max=4,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=55, y=75,
        radius=25,
        thickness=4,
        start_angle=-90,
        end_angle=180
    },
    {
        name='loadavg',
        arg='2',
        max=4,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=55, y=75,
        radius=20,
        thickness=4,
        start_angle=-90,
        end_angle=180
    },
    {
        name='loadavg',
        arg='3',
        max=4,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=55, y=75,
        radius=15,
        thickness=4,
        start_angle=-90,
        end_angle=180
    },
    {
        name='memperc',
        arg='',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=80, y=125,
        radius=25,
        thickness=4,
        start_angle=-90,
        end_angle=180
    },
    {
        name='swapperc',
        arg='',
        max=100,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=80, y=125,
        radius=20,
        thickness=4,
        start_angle=-90,
        end_angle=180
    },
    {
        name='downspeedf',
        arg='gw_iface',
        max=1024,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=140, y=125,
        radius=25,
        thickness=4,
        start_angle=-180,
        end_angle=90
    },
    {
        name='upspeedf',
        arg='gw_iface',
        max=1024,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=140, y=125,
        radius=20,
        thickness=4,
        start_angle=-180,
        end_angle=90
    },
    {
        name='hwmon',
        arg='0 temp 2',
        max=95,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=166, y=75,
        radius=25,
        thickness=4,
        start_angle=-180,
        end_angle=90
    },
    {
        name='hwmon',
        arg='0 temp 4',
        max=95,
        bg_colour=0xffffff,
        bg_alpha=0.2,
        fg_colour=0x0866ff,
        fg_alpha=0.8,
        x=166, y=75,
        radius=20,
        thickness=4,
        start_angle=-180,
        end_angle=90
    }
}

require 'cairo'

function rgb_to_r_g_b(colour,alpha)
    return ((colour / 0x10000) % 0x100) / 255., ((colour / 0x100) % 0x100) / 255., (colour % 0x100) / 255., alpha
end

function draw_ring(cr,t,pt)
    local w,h=conky_window.width,conky_window.height

    local xc,yc,ring_r,ring_w,sa,ea=pt['x'],pt['y'],pt['radius'],pt['thickness'],pt['start_angle'],pt['end_angle']
    local bgc, bga, fgc, fga=pt['bg_colour'], pt['bg_alpha'], pt['fg_colour'], pt['fg_alpha']

    local angle_0=sa*(2*math.pi/360)-math.pi/2
    local angle_f=ea*(2*math.pi/360)-math.pi/2
    local t_arc=t*(angle_f-angle_0)

    -- Draw background ring

    cairo_arc(cr,xc,yc,ring_r,angle_0,angle_f)
    cairo_set_source_rgba(cr,rgb_to_r_g_b(bgc,bga))
    cairo_set_line_width(cr,ring_w)
    cairo_stroke(cr)

    -- Draw indicator ring

    cairo_arc(cr,xc,yc,ring_r,angle_0,angle_0+t_arc)
    cairo_set_source_rgba(cr,rgb_to_r_g_b(fgc,fga))
    cairo_stroke(cr)
end

function conky_clock_rings()
    local function setup_rings(cr,pt)
        local str=''
        local value=0

        str=string.format('${%s %s}',pt['name'],pt['arg'])
        str=conky_parse(str)

        value=tonumber(str) or 0
        pct=value/pt['max']

        draw_ring(cr,pct,pt)
    end

    -- Check that Conky has been running for at 1 update

    if conky_window==nil then return end
    local cs=cairo_xlib_surface_create(conky_window.display,conky_window.drawable,conky_window.visual, conky_window.width,conky_window.height)

    local cr=cairo_create(cs)

    local updates=conky_parse('${updates}')
    update_num=tonumber(updates)

    if update_num>1 then
        for i in pairs(settings_table) do
            setup_rings(cr,settings_table[i])
        end
    end
end
