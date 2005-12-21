/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: UI_NavTab.h,v $
 * Date modified: $Date: 2005-09-23 12:56:51 -0500 (Fri, 23 Sep 2005) $
 * Version:       $Rev: 3082 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_NAV_H_
#define _VE_UI_NAV_H_

#include "VE_Open/skel/VjObsC.h"

#include <wx/image.h>
#include <wx/dialog.h>
#include <wx/bmpbuttn.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/statbmp.h>
#include <wx/scrolwin.h>

#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

#include <vector>

class wxCheckBox;
class wxSlider;
class wxButton;
class wxWindow;
class wxSizer;

namespace VE_XML
{
   class VECommand;
}

namespace VE_Conductor
{
   class DOMDocumentManager;
}

enum NAV_TAB_IDS 
{
   LEFT_B,
   RIGHT_B,
   UP_B,
   DOWN_B,
   FORWARD_B,
   BACKWARD_B,
   CCW_B,
   CW_B,
   TRANS_STEP_SLIDER,
   ROT_STEP_SLIDER,
   HEAD_ROTATE_CHK,
   RESET_NAV_POSITION
   //NONE= -1000
};

//override the buttons
class UI_NavButton: public wxBitmapButton
{
public:
   UI_NavButton(wxWindow* parent, wxWindowID id, const wxBitmap& bitmap);
   virtual ~UI_NavButton(){;}

   //need to override this function
   void onMouse(wxMouseEvent& event);
   void onMouseUp(wxMouseEvent& event);

protected:
   int _buttonPushed;
   DECLARE_EVENT_TABLE()
};

class UI_NavigateScroll: public wxScrolledWindow
{
public:
   UI_NavigateScroll(wxWindow* parent);
   ~UI_NavigateScroll();

   UI_NavButton* _leftButton;
   UI_NavButton* _rightButton;
   UI_NavButton* _upButton;
   UI_NavButton* _downButton;
   UI_NavButton* _forwardButton;
   UI_NavButton* _backButton;
   UI_NavButton* _pitchupButton;
   UI_NavButton* _pitchdownButton;
   UI_NavButton* _rollccwButton;
   UI_NavButton* _rollcwButton;
   UI_NavButton* _yawccwButton;
   UI_NavButton* _yawcwButton;

   wxSlider*   translationStepSize;
   wxSlider*   rotationStepSize;
   wxCheckBox* headRotationChk;
   wxButton*   resetNavPosition;
protected:
   wxImage* _image1;
   wxImage* _image2;
   wxImage* _image3;
   wxImage* _image4;
   wxImage* _image5;
   wxImage* _image6;
   wxImage* _image7;
   wxImage* _image8;
   wxImage* _image9;
   wxImage* _image10;
   wxImage* _image11;
   wxImage* _image12;
   wxImage* _imagecoord;

   wxBitmap* _bitmap1;
   wxBitmap* _bitmap2;
   wxBitmap* _bitmap3;
   wxBitmap* _bitmap4;
   wxBitmap* _bitmap5;
   wxBitmap* _bitmap6;
   wxBitmap* _bitmap7;
   wxBitmap* _bitmap8;
   wxBitmap* _bitmap9;
   wxBitmap* _bitmap10;
   wxBitmap* _bitmap11;
   wxBitmap* _bitmap12;
   wxBitmap* _bitmapcoord;

   DECLARE_EVENT_TABLE()
};

//the main navigation tab class
class NavigationPane : public wxDialog 
{
public:
   NavigationPane( VjObs_ptr veEngine, VE_Conductor::DOMDocumentManager* domManagerIn );
   virtual ~NavigationPane(){;}

   //turn off the navigation flag
   void onLeftMouseUp(wxMouseEvent& event);
   void onLeftDown(wxMouseEvent& event);

   //mouse callback
   void onMouse(wxMouseEvent& mouse);

   //update the tab that we're moving/stopping
   void updateParent(int push, int id);
  
   //add a button to the managed list
   void setActiveButton(int id){_activeButton = id;}
   int getActiveButton(){return _activeButton;}
   
   void OnTransStepSlider( wxScrollEvent& event);
   void OnRotStepSlider( wxScrollEvent& event);
   void OnResetNavPosition( wxCommandEvent& event );
   void OnHeadCheck( wxCommandEvent& event );
   void SetCommInstance( VjObs_ptr veEngine );
   //void SetDOMManager( VE_Conductor::DOMDocumentManager* domManagerIn );
   void SendCommandsToXplorer( void );

protected:
   int _activeButton;
   UI_NavigateScroll* navScroll;
   std::vector< VE_XML::VECommand* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value;
   DOMDocument* doc;
   VE_Conductor::DOMDocumentManager* domManager;
   std::string dataValueName;

   DECLARE_EVENT_TABLE()
};
#endif  //_VE_UI_NAV_H_

