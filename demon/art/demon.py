#!/usr/bin/env python3
"""Writes the demon SVGs (normal, cold, scorched) and the heat icons."""

def demon(body, belly, shade, outline, wing, eyes, extra_back="", extra_front="", brow="mean", mouth="grin"):
    brows = {
        "mean":   '<path d="M41 47 L60 54" /><path d="M89 47 L71 54" />',
        "worried":'<path d="M42 54 L59 47" /><path d="M88 54 L72 47" />',
    }[brow]
    mouths = {
        "grin": f'<path d="M50 82 Q 66 96 84 80" fill="none" stroke="{outline}" stroke-width="3.5" stroke-linecap="round"/>'
                f'<path d="M72 88 L76 96 L79 86 Z" fill="#ffffff" stroke="{outline}" stroke-width="1.5" stroke-linejoin="round"/>',
        "wavy": f'<path d="M50 88 Q 55 82 60 88 Q 65 94 70 88 Q 75 82 80 88" fill="none" stroke="{outline}" stroke-width="3.5" stroke-linecap="round"/>',
    }
    return f'''<svg xmlns="http://www.w3.org/2000/svg" width="128" height="128" viewBox="0 0 128 128">
  {extra_back}
  <g stroke="{outline}" stroke-width="3.2" stroke-linejoin="round" stroke-linecap="round">
    <!-- tail -->
    <path d="M30 96 C 10 96 6 74 16 62" fill="none" stroke-width="5"/>
    <path d="M16 62 L6 64 L14 48 L24 62 Z" fill="{shade}"/>
    <!-- wings -->
    <path d="M30 66 C 18 54 10 50 4 52 C 10 58 8 64 12 68 C 16 66 20 70 18 76 C 24 74 28 76 30 80 Z" fill="{wing}"/>
    <path d="M98 66 C 110 54 118 50 124 52 C 118 58 120 64 116 68 C 112 66 108 70 110 76 C 104 74 100 76 98 80 Z" fill="{wing}"/>
    <!-- horns -->
    <path d="M42 42 C 32 30 32 16 40 6 C 42 18 50 26 56 34 Z" fill="#f4e7c8"/>
    <path d="M86 42 C 96 30 96 16 88 6 C 86 18 78 26 72 34 Z" fill="#f4e7c8"/>
    <!-- feet -->
    <ellipse cx="48" cy="114" rx="11" ry="6" fill="{shade}"/>
    <ellipse cx="80" cy="114" rx="11" ry="6" fill="{shade}"/>
    <!-- body -->
    <path d="M64 28 C 92 28 106 50 106 76 C 106 100 90 114 64 114 C 38 114 22 100 22 76 C 22 50 36 28 64 28 Z" fill="{body}"/>
  </g>
  <ellipse cx="66" cy="90" rx="25" ry="18" fill="{belly}"/>
  <path d="M34 50 C 40 38 52 33 62 33" fill="none" stroke="#ffffff" stroke-opacity="0.35" stroke-width="5" stroke-linecap="round"/>
  {eyes}
  <g stroke="{outline}" stroke-width="4" stroke-linecap="round" fill="none">{brows}</g>
  {mouths[mouth]}
  {extra_front}
</svg>'''

def open_eyes(iris, outline):
    return f'''<g stroke="{outline}" stroke-width="2.5">
    <ellipse cx="51" cy="63" rx="11" ry="12.5" fill="#fffaf0"/>
    <ellipse cx="78" cy="63" rx="11" ry="12.5" fill="#fffaf0"/>
  </g>
  <circle cx="54" cy="64" r="7" fill="{iris}"/><circle cx="81" cy="64" r="7" fill="{iris}"/>
  <ellipse cx="55" cy="64" rx="2.4" ry="5" fill="#140a1f"/><ellipse cx="82" cy="64" rx="2.4" ry="5" fill="#140a1f"/>
  <circle cx="51" cy="60" r="2" fill="#ffffff"/><circle cx="78" cy="60" r="2" fill="#ffffff"/>'''

def x_eyes(outline):
    return f'''<g stroke="{outline}" stroke-width="4.5" stroke-linecap="round">
    <path d="M44 56 L58 70"/><path d="M58 56 L44 70"/>
    <path d="M71 56 L85 70"/><path d="M85 56 L71 70"/>
  </g>'''

normal = demon("#9a5cf5", "#c9a6ff", "#6b37c4", "#1a0f2e", "#5a2aa8",
               open_eyes("#ffc93c", "#1a0f2e"))

icicles = '''<g fill="#e8f8ff" stroke="#0d2a40" stroke-width="2" stroke-linejoin="round">
    <path d="M44 106 L48 122 L52 106 Z"/><path d="M60 110 L63 126 L67 110 Z"/><path d="M76 108 L80 121 L84 107 Z"/>
  </g>
  <g fill="#ffffff"><path d="M98 34 l2 6 l6 2 l-6 2 l-2 6 l-2 -6 l-6 -2 l6 -2 Z"/><path d="M30 30 l1.5 4 l4 1.5 l-4 1.5 l-1.5 4 l-1.5 -4 l-4 -1.5 l4 -1.5 Z"/></g>
  <path d="M30 40 C 40 30 56 26 70 28 C 60 32 52 30 46 38 C 40 36 36 40 30 40 Z" fill="#ffffff" opacity="0.85"/>'''
cold = demon("#7cc6f2", "#dff4ff", "#3d8cc4", "#0d2a40", "#4a9ad0",
             open_eyes("#2f6fb0", "#0d2a40"), extra_front=icicles, brow="worried", mouth="wavy")

smoke = '''<g fill="#4a4048" opacity="0.75">
    <circle cx="54" cy="18" r="8"/><circle cx="64" cy="10" r="10"/><circle cx="76" cy="18" r="7"/>
  </g>'''
scorch = '''<g fill="#2b0f08" opacity="0.55"><circle cx="40" cy="96" r="5"/><circle cx="92" cy="84" r="4"/><circle cx="84" cy="102" r="3"/></g>'''
hot = demon("#ff5a2a", "#ffb37a", "#b8321c", "#3a0d05", "#a8281a",
            x_eyes("#3a0d05"), extra_back=smoke, extra_front=scorch, brow="worried", mouth="wavy")

open("demon.svg", "w").write(normal)
open("demon_cold.svg", "w").write(cold)
open("demon_hot.svg", "w").write(hot)

# Heat markers drawn on hot and cold blocks.
flame = '''<svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" viewBox="0 0 64 64">
  <path d="M32 4 C 36 16 50 22 50 40 C 50 52 42 60 32 60 C 22 60 14 52 14 40 C 14 30 20 26 22 18 C 26 24 28 28 28 32 C 32 24 34 14 32 4 Z"
        fill="#fff3c4" stroke="#7a1f05" stroke-width="3" stroke-linejoin="round"/>
  <path d="M32 30 C 36 38 42 42 42 48 C 42 54 37 57 32 57 C 27 57 22 54 22 48 C 22 42 28 40 32 30 Z" fill="#ffb02e"/>
</svg>'''
snow = '''<svg xmlns="http://www.w3.org/2000/svg" width="64" height="64" viewBox="0 0 64 64">
  <g stroke="#0b2f52" stroke-width="9" stroke-linecap="round">
    <path d="M32 6 V58"/><path d="M9.5 19 L54.5 45"/><path d="M9.5 45 L54.5 19"/>
  </g>
  <g stroke="#f2fbff" stroke-width="4.5" stroke-linecap="round" fill="none">
    <path d="M32 6 V58"/><path d="M9.5 19 L54.5 45"/><path d="M9.5 45 L54.5 19"/>
    <path d="M25 9 L32 16 L39 9"/><path d="M25 55 L32 48 L39 55"/>
    <path d="M8 28 L17 24 L14 15"/><path d="M56 36 L47 40 L50 49"/>
    <path d="M8 36 L17 40 L14 49"/><path d="M56 28 L47 24 L50 15"/>
  </g>
</svg>'''
open("flame.svg", "w").write(flame)
open("snow.svg", "w").write(snow)
