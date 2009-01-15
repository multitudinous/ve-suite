/************************************************************************
* Author(s):     Joshua Doss                                            *
* Last Modified: 10/31/2005                                             *
************************************************************************/
/************************************************************************
*                                                                       *
*               Copyright (C) 2002-2005  3Dlabs Inc. Ltd.               *
*                                                                       *
*                        All rights reserved.                           *
*                                                                       *
* Redistribution and use in source and binary forms, with or without    *
* modification, are permitted provided that the following conditions    *
* are met:                                                              *
*                                                                       *
*     Redistributions of source code must retain the above copyright    *
*     notice, this list of conditions and the following disclaimer.     *
*                                                                       *
*     Redistributions in binary form must reproduce the above           *
*     copyright notice, this list of conditions and the following       *
*     disclaimer in the documentation and/or other materials provided   *
*     with the distribution.                                            *
*                                                                       *
*     Neither the name of 3Dlabs Inc. Ltd. nor the names of its         *
*     contributors may be used to endorse or promote products derived   *
*     from this software without specific prior written permission.     *
*                                                                       *
* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS   *
* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT     *
* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS     *
* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE        *
* COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, *
* INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,  *
* BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;      *
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER      *
* CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT    *
* LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN     *
* ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE       *
* POSSIBILITY OF SUCH DAMAGE.                                           *
*                                                                       *
************************************************************************/

# pragma once

#include "Compulsory.h"
#include <wx/msgdlg.h>

class SGOglNotebook;

class SGOglTextureCoordNBPage: public wxPanel
{
public:
    SGOglTextureCoordNBPage(SGOglNotebook*  parent, wxWindowID id);

    void OnRadioTexCoordGen(wxCommandEvent &event);
    void OnRadioTextureCoordUnit(wxCommandEvent &event);
    void OnCheckbox(wxCommandEvent &evt);
    void OnTextEnterEyeCoeffS(wxCommandEvent &event);
    void OnTextEnterEyeCoeffT(wxCommandEvent &event);
    void OnTextEnterObjCoeffS(wxCommandEvent &event);
    void OnTextEnterObjCoeffT(wxCommandEvent &event);

    DECLARE_EVENT_TABLE()

private:

    enum TextureCoordinateGenerationMethod {
        TEXTURE_COORDINATE_OBJECT_LINEAR = 0,
        TEXTURE_COORDINATE_EYE_LINEAR,
        TEXTURE_COORDINATE_SPHERE_MAP,
        TEXTURE_COORDINATE_REFLECTION_MAP,
        TEXTURE_COORDINATE_NORMAL_MAP
    };

    SGOglNotebook *m_parent;

    wxCheckBox *tex0TexGenEnableCheckBox, *tex1TexGenEnableCheckBox, *tex2TexGenEnableCheckBox, *tex3TexGenEnableCheckBox, *tex4TexGenEnableCheckBox;
    
    wxRadioBox *coordGenBox, *texCoordUnitBox;
    
    wxTextCtrl *eyePlaneCoeffTextS, *eyePlaneCoeffTextT, *objectPlaneCoeffTextS, *objectPlaneCoeffTextT;

    void UpdateWidgets();
};
