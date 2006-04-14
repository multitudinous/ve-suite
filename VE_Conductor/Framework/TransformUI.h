/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _TRANSFORM_UI_H
#define _TRANSFORM_UI_H
#include <wx/panel.h>
#include <wx/spinctrl.h>

class wxSpinCtrlDbl;

namespace VE_XML
{
class Transform;
}

namespace VE_Conductor
{
namespace GUI_Utilities
{
class TransformUI : public wxPanel
{
public:
   ///Enums used by wxwidgets
   enum TRANSFORM_UI
   {
      TRANSFORM_PANEL_ID,///<The transform panel ID.
   };
   ///Constructor
   TransformUI( wxWindow* parent, wxString dialogName, VE_XML::Transform* transform );
   ///Destructor
   virtual ~TransformUI();
   ///Callback for the transform ui
   void UpdateTransform( wxSpinEvent& event );
   ///Get the current transform
   //VE_XML::Transform* GetTransform( void );
private:
   VE_XML::Transform* transform;///<The Trasnform for the ui.

   ///Transform panel controls
   wxSpinCtrlDbl* _xTransformCtrl;///<X translation control
   wxSpinCtrlDbl* _yTransformCtrl;///<Y translation control
   wxSpinCtrlDbl* _zTransformCtrl;///<Z translation control

   wxSpinCtrlDbl* _xRotationCtrl;///<X rotation control
   wxSpinCtrlDbl* _yRotationCtrl;///<Y rotation control
   wxSpinCtrlDbl* _zRotationCtrl;///<Z rotation control

   wxSpinCtrlDbl* _xScaleCtrl;///<X scale control
   wxSpinCtrlDbl* _yScaleCtrl;///<Y scale control
   wxSpinCtrlDbl* _zScaleCtrl;///<Z scale control

   DECLARE_EVENT_TABLE()
};
}
}
#endif //_TRANSFORM_UI_H
