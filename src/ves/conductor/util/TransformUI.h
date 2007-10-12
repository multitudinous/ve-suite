/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _TRANSFORM_UI_H
#define _TRANSFORM_UI_H
/*!\file TransformUI.h
TransformUI API
*/
/*!\class ves::conductor::util::TransformUI
* 
*/
#include <wx/panel.h>
#include <wx/spinctrl.h>
#include <vector>
#include <wx/textctrl.h>
#include <wx/statbox.h>
#include <wx/sizer.h>

#include <string>
#include <ves/VEConfig.h>
class DataSetLoaderUI;
class wxSpinCtrlDbl;
class wxCheckBox;

namespace ves
{
namespace open
{
namespace xml
{
   class Transform;
   class DataValuePair;
   class ParameterBlock;
   namespace model
   {
      class Model;
   }
}
}
}

namespace ves
{
namespace conductor
{
namespace util
{
      class VE_CONDUCTOR_UTILS_EXPORTS TransformUI : public wxPanel
      {
      public:
         ///Constructor
		  TransformUI( wxWindow* parent, wxString dialogName, ves::open::xml::Transform* transform );
         ///Destructor
         virtual ~TransformUI();

         ///Enums used by wxwidgets
         enum TRANSFORM_UI
         {
            TRANSFORM_PANEL_ID,///<The transform panel ID.
            UNIFORM_SCALE///<The scale uniformly ID
         };

         ///Callback for the transform ui
         void UpdateTransform( wxSpinEvent& event );

         ///Update whether uniform scaling is used
         ///\param event The wxCommand event
         void UpdateUniformScale( wxCommandEvent& event );

         ///Get the current transform
         //ves::open::xml::Transform* GetTransform( void );

         ///Get the current parameter block id
         void SetParamBlockID( std::string id );
         ///Get the current parameter block transform
         void SetParamBlockTransform( ves::open::xml::Transform* transform );

      private:
         ves::open::xml::Transform* transform;///<The Trasnform for the ui.

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

         wxCheckBox* m_uniformScale;///<Uniform scaling checkbox

         std::string _commandName;///<The command name.
         std::vector<ves::open::xml::DataValuePair*> _instructions;///<The DataValuePair s for the current command.

         std::string _id;///<parameter block id
         ves::open::xml::Transform* _transform;///<parameter block transform

         ///Send the Command back to VE-Xplorer.
         void _sendCommandsToXplorer();

         double tempX;///<The x scale value.
         double tempY;///<The y scale value.
         double tempZ;///<The z scale value.

         DataSetLoaderUI* dataset;
         ves::open::xml::ParameterBlock* paramBlock;

         DECLARE_EVENT_TABLE()
      };
}
}
}
#endif //_TRANSFORM_UI_H
