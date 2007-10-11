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

#ifndef _ADVANCEDVECTORS_H_
#define _ADVANCEDVECTORS_H_
/*!\file advancedvectors.h
*advancedvectors API
*/
/*!\class AdvancedVectors
* 
*/
#include <ves/conductor/util/DualSlider.h>

#include <ves/VEConfig.h>
#include <vector>

#include <wx/dialog.h>

namespace VE_XML
{
   class Command;
}

class wxSlider;
class wxCheckBox;


#define ID_DIALOG 10000
#define SYMBOL_ADVANCEDVECTORS_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_ADVANCEDVECTORS_TITLE _T("Advanced Vectors")
#define SYMBOL_ADVANCEDVECTORS_IDNAME ID_DIALOG
#define SYMBOL_ADVANCEDVECTORS_SIZE wxSize(400, 300)
#define SYMBOL_ADVANCEDVECTORS_POSITION wxDefaultPosition

class VE_GUIPLUGINS_EXPORTS AdvancedVectors: public wxDialog
{    

public:
   /// Constructors
   AdvancedVectors( );
   AdvancedVectors( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDVECTORS_IDNAME, 
                  const wxString& caption = SYMBOL_ADVANCEDVECTORS_TITLE,
                  const wxPoint& pos = SYMBOL_ADVANCEDVECTORS_POSITION, 
                  const wxSize& size = SYMBOL_ADVANCEDVECTORS_SIZE,
                  long style = SYMBOL_ADVANCEDVECTORS_STYLE );

   enum ADVANCED_VECTOR_IDS
   {
      VECTOR_MAX_SLIDER,
      VECTOR_MIN_SLIDER,
      VECTOR_SCALE_SLIDER,
      VECTOR_RATIO_SLIDER,
      SCALAR_BY_VECTOR_CHK
   };

   bool Create( wxWindow* parent, wxWindowID id = SYMBOL_ADVANCEDVECTORS_IDNAME, 
              const wxString& caption = SYMBOL_ADVANCEDVECTORS_TITLE, 
              const wxPoint& pos = SYMBOL_ADVANCEDVECTORS_POSITION, 
              const wxSize& size = SYMBOL_ADVANCEDVECTORS_SIZE, 
              long style = SYMBOL_ADVANCEDVECTORS_STYLE );

   /// Creates the controls and sizers
   void CreateControls();

    ///Set the vector threshold
   ///\param range The vector threshold to set.
   void SetVectorThreshold(std::vector<double> range);

   ///Get the vector threshold
   ///\param range The vector threshold to fill in.
   void GetVectorThreshold(std::vector<double>& range);

   ///Set the vector scale.
   ///\param value The vector scale.
   void SetVectorScale(double value);

   ///Set the vector ratio.
   ///\param value The vector ratio.
   void SetVectorRatio(double value);

   ///Set the scale by magnitude flag.
   ///\param value Flag determining how to scale vectors.
   void SetScaleByMagFlag(bool value);

   ///Get the vector scale
   double GetVectorScale();

   ///Get the vector ratio.
   double GetVectorRatio();

   ///Get the scale by magnitude flag.
   bool GetScaleByMagFlag();


   /// Retrieves bitmap resources
   wxBitmap GetBitmapResource( const wxString& name );

   /// Retrieves icon resources
   wxIcon GetIconResource( const wxString& name );

   /// Should we show tooltips?
   static bool ShowToolTips();

protected:

   VE_Conductor::GUI_Utilities::DualSlider* vectorRange;
   wxSlider* _vectorScaleSlider;///<Slider widget for vector scale.
   wxSlider*   _vectorRatioSlider;///<Slider widget for vector ratio.
   wxCheckBox* _scaleByMagCheck;///<Check box widget for scale option.
   
};

#endif
    // _VECTORS_H_
