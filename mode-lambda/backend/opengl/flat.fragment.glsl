/*  CRT shader

    Copyright (C) 2010-2012 cgwg, Themaister and DOLLS

    This program is free software; you can redistribute it and/or modify it
    under the terms of the GNU General Public License as published by the Free
    Software Foundation; either version 2 of the License, or (at your option)
    any later version.
*/
#version 330

uniform sampler2D rubyTexture;
uniform vec2 rubyInputSize;
uniform vec2 rubyTextureSize;

in vec2 texCoord;

out vec4 oFragColor;

void main() {
  oFragColor = texelFetch(rubyTexture, ivec2(texCoord), 0);
}
