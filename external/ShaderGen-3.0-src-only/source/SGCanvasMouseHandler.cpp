/************************************************************************
* Author(s):    Inderaj Bains and Joshua Doss                           *
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

#include "SGCanvasMouseHandler.h"
#include "SGCanvas.h"
#include "SGFrame.h"

const float SGCanvasMouseHandler::StartZoom = 0.8f;
const float SGCanvasMouseHandler::InertiaThreshold = 1.0f;
const int SGCanvasMouseHandler::Delay = 10;

SGCanvasMouseHandler::SGCanvasMouseHandler()
{
    canvas = 0;
    Reset();
}

SGCanvasMouseHandler::SGCanvasMouseHandler(SGCanvas * canvas1)
{
    canvas = canvas1;
    Reset();
}

void SGCanvasMouseHandler::Reset()
{
    previous = wxGetElapsedTime(true);
    frames = 0.0f;

    startZoom = StartZoom;
    if (canvas)
    {
        canvas->SetZoom(startZoom);
    }
    Stop();
    vPrev = vec3(0.0f, 0.0f, 0.0f);
    vInc = vec3(0.0f, 0.0f, 0.0f);
    inertiaTheta = 0.0f;
    inertiaAxis = vec3(0.0f, 1.0f, 0.0f);
    validStart = false;
    xform.identity();
    wxStartTimer();
}

void SGCanvasMouseHandler::Stop()
{
    inertiaTheta = 0.0f;
    vInc = vec3(0.0f, 0.0f, 0.0f);
}

void SGCanvasMouseHandler::OnMouse(wxMouseEvent& event)
{
    vec3 cursor = canvas->GetWorldSpace(event.GetX(), event.GetY());

    if (event.LeftDown())
    {
        if (!event.ControlDown())
        {
            Stop();
        }
        vStart = cursor;
        memcpy((float*) &mStart, (float*) &xform, sizeof(xform));
        startZoom = canvas->GetZoom();
        vPrev = cursor;
        validStart = true;
    }
    else if (event.LeftUp() && validStart)
    {
        float theta = 180 * vInc.magnitude();
        if (theta < -InertiaThreshold || theta > InertiaThreshold)
        {
            inertiaAxis = cross(vStart, cursor);
            if (inertiaAxis.magnitude())
            {
                inertiaAxis.unitize();
                inertiaTheta = theta;
            }
        }
        else if (!event.ControlDown())
        {
            Stop();
        }
        validStart = false;
    }
    else if (event.Moving() && event.LeftIsDown())
    {
        if (!validStart)
        {
            vInc = vec3(0.0f, 0.0f, 0.0f);
        }
        else
        {
            if (event.ControlDown())
            {
                float delta = cursor.y - vStart.y;
                if (delta)
                {
                    canvas->SetZoom(startZoom + delta);
                    canvas->Update();
                }
            }
            else
            {
                float theta = 180 * (cursor - vStart).magnitude();
                if (theta)
                {
                    vec3 axis = cross(vStart, cursor);
                    axis.unitize();

                    glLoadIdentity();
                    glRotatef(-theta, axis.x, axis.y, axis.z);
                    glMultMatrixf((float*) &mStart);
                    glGetFloatv(GL_MODELVIEW_MATRIX, (float*) &xform);
                    canvas->Update();
                }
            }
            vInc = cursor - vPrev;
        }
        vPrev = cursor;
    }

    // Right mouse button zooms.
    else if (event.RightDown())
    {
        vStart = cursor;
        startZoom = canvas->GetZoom();
        validStart = true;
    }
    else if (event.RightUp() && validStart)
    {
        validStart = false;
    }
    else if (event.Moving() && event.RightIsDown())
    {
        if (validStart)
        {
            float delta = cursor.y - vStart.y;
            if (delta)
            {
                canvas->SetZoom(startZoom + delta);
                canvas->Update();
            }
        }
    }

    event.Skip();
}

void SGCanvasMouseHandler::LoadMatrix() const
{
    glLoadIdentity();

    if (canvas->GetFrame()->IsChecked(Id::ViewPerspective))
    {
        glTranslatef(0.0f, 0.0f, SGCanvas::CameraZ - 1.0f);
    }
    else
    {
        glTranslatef(0.0f, 0.0f, SGCanvas::CameraZ);
    }

    glRotatef(20.0f, 1.0f, 0.0f, 0.0f);
    MultMatrix();
}
