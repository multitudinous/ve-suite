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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VISTAB_H_
#define _VISTAB_H_
/*!\file vistab.h
*Vistab API
*/
/*!\class Vistab
* 
*/
#include "VE_Open/skel/VjObsC.h"

#include <wx/toolbar.h>
#include <wx/dialog.h>

#include "VE_Conductor/GUIPlugin/DualSlider.h"

#include <vector>
#include <map>
#include <string>

namespace VE_XML
{
   class Command;
   class DataValuePair;
}
namespace VE_Model
{
   class Model;
}

class Contours;
class Streamlines;
class Isosurfaces;
class TextureBasedToolBar;
class Polydata;

class wxComboBox;
class wxListBox;
class wxSpinCtrlDbl;
class wxCheckBox;
class wxTextCtrl;
class wxButton;

#define ID_DIALOG 10000
#define SYMBOL_VISTAB_STYLE wxCAPTION|wxRESIZE_BORDER|wxSYSTEM_MENU|wxCLOSE_BOX
#define SYMBOL_VISTAB_TITLE _T("VE-Suite")
#define SYMBOL_VISTAB_IDNAME ID_DIALOG
#define SYMBOL_VISTAB_SIZE wxSize(400, 300)
#define SYMBOL_VISTAB_POSITION wxDefaultPosition
#define ID_TOOLBAR 10001
#define ID_TOOL 10002
#define ID_TOOL1 10003
#define ID_TOOL2 10004
#define ID_TOOL3 10005
#define ID_TOOL4 10006
#define ID_TOOL5 10007
#define ID_COMBOBOX 10010
#define ID_LISTBOX 10011
#define ID_LISTBOX1 10012
#define ID_BUTTON 10015
#define ID_COMBOBOX1 10016

////@end control identifiers


#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif


class Vistab: public wxDialog
{    
public:
   ///Constructor
   ///\param activeModel The active Model information from CORBA.
   Vistab(VjObs::Model_var activeModel );

   ///Constructor
   ///\param activeModel The active Model information from CORBA.
   Vistab(VjObs::Model_var activeModel,
            wxWindow* parent,
            wxWindowID id = SYMBOL_VISTAB_IDNAME,
            const wxString& caption = SYMBOL_VISTAB_TITLE,
            const wxPoint& pos = SYMBOL_VISTAB_POSITION,
            const wxSize& size = SYMBOL_VISTAB_SIZE,
            long style = SYMBOL_VISTAB_STYLE );
   virtual ~Vistab();

   enum VISTAB_IDS
   {
      CONTOUR_DLG,
      VECTOR_DLG,
      STREAMLINE_DLG,
      ISOSURFACE_DLG,
      TEXTURE_BASED_DLG,
      POLYDATA_DLG,
      CONTOUR_BUTTON,
      VECTOR_BUTTON,
      STREAMLINE_BUTTON,
      ISOSURFACE_BUTTON,
      TEXTURE_BASED_BUTTON,
      POLYDATA_BUTTON,
      ID_CLEAR_ALL_BUTTON,
      MIN_SPINCTRL,
      MAX_SPINCTRL,
      MIN_MAX_SLIDERS,
      MIN_SLIDER,
      MAX_SLIDER,
      CLOSE_BUTTON,
      ID_DATA_WIREFRAME_CB,
      ID_DATA_BBOX_CB,
      ID_DATA_AXES_CB,
      ID_DATA_UPDATE_AXES,
      ID_DATA_SCALAR_BAR
   };

    void SendCommandsToXplorer( void );
    /// Creation
    bool Create( wxWindow* parent,
                 wxWindowID id = SYMBOL_VISTAB_IDNAME,
                 const wxString& caption = SYMBOL_VISTAB_TITLE,
                 const wxPoint& pos = SYMBOL_VISTAB_POSITION, 
                 const wxSize& size = SYMBOL_VISTAB_SIZE, 
                 long style = SYMBOL_VISTAB_STYLE );

    /// Creates the controls and sizers
    void CreateControls();


    /// Retrieves bitmap resources
    wxBitmap GetBitmapResource( const wxString& name );

    /// Retrieves icon resources
    wxIcon GetIconResource( const wxString& name );

    /// Should we show tooltips?
    static bool ShowToolTips();

   ///Set the active model for this dialog
   ///\param activeModel The current model.
   void SetActiveModel(VjObs::Model_var activeModel);

   ///Set the active dataset
   ///\param name The name of the dataset
   void SetActiveDataset(std::string name);

   ///Send the current vistab data to xplorer.
   ///\param subDialogCommand The setting from any of the sub dialogs.
   void SendUpdatedSettingsToXplorer(VE_XML::Command* subDialogCommand=0);

   ///Clear out the DataValuePair(s) of basic info about the vistab
   void ClearBaseInformation();
   
   ///Clear out the DataValuePair(s) of specific info about the sub dialogs
   void ClearSpecificInformation();

   ///Set the texture data information
   ///\param textureData The directory names for the texture data
   ///\param type The type of data\n Valid types are:\n TEXTURE_SCALARS\n TEXTURE_VECTORS
   void SetTextureData(wxArrayString textureData,std::string type);

   ///Get active scalar name
   std::string GetActiveScalarName();
   ///Get active vector name
   std::string GetActiveVectorName();
   ///Get active dataset name
   std::string GetActiveDatasetName();

   ///Get the scalar range sliders  
   VE_Conductor::GUI_Utilities::DualSlider* GetScalarRangeControls();

   //Vectors* vector;
   //Contours* contour;
   //Streamlines* streamline;
   //Isosurfaces* isosurface;
   //TextureBasedToolBar* _tbTools;///<TextureBasedToolBar.
   VE_Conductor::GUI_Utilities::DualSlider* scalarRange;

   ///Make sure all the dataset check boxes are unchecked.\n
   ///This occurs when loading a new network
   void ResetAllDatasetDependentCheckBoxes();

   wxToolBar*  itemToolBar3;
   wxComboBox* itemComboBox11;
   wxComboBox* itemComboBox12; 
   wxListBox*  itemListBox13; 
   wxListBox*  itemListBox15;
   wxSpinCtrlDbl* _minSpinner;
   wxSpinCtrlDbl* _maxSpinner;
   wxSlider* _minSlider;
   wxSlider* _maxSlider;
   wxString* _none;

   ///Updates the spin controllers
   void UpdateSpinControls( void );

protected:
   /*!\class ScalarRangeMinSliderCallback
    *Class that allows the user to do operations based on the min slider events
    */
   class ScalarRangeMinSliderCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
         ScalarRangeMinSliderCallback(Vistab* parent){_parent = parent;}
        ///Destructor
        virtual ~ScalarRangeMinSliderCallback(){}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         Vistab* _parent;
   };
   /*!\class ScalarRangeBothMoveCallback
    *Class that allows the user to do operations based on both sliders moving, i.e.
    *This is caused by the slider buffer being reached.
    */
   class ScalarRangeBothMoveCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ScalarRangeBothMoveCallback(Vistab* parent){_parent = parent;}
      
        ///Destructor
        virtual ~ScalarRangeBothMoveCallback(){}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         Vistab* _parent;
   };
   /*!\class ScalarRangeMaxSliderCallback
    *Class that allows the user to do operations based on the max slider events
    */
   class ScalarRangeMaxSliderCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ScalarRangeMaxSliderCallback(Vistab* parent){_parent = parent;}
        ///Destructor
        virtual ~ScalarRangeMaxSliderCallback(){}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         Vistab* _parent;
   };
   void _onContour(wxCommandEvent& );
   void _onVector(wxCommandEvent& );
   void _onStreamline(wxCommandEvent& );
   void _onIsosurface(wxCommandEvent& );
   void _onTextureBased(wxCommandEvent& );
   void _onPolydata(wxCommandEvent& );

   void UpdateBoundingBox( wxCommandEvent& event );
   void UpdateWireFrame( wxCommandEvent& event );
   void UpdateAxes( wxCommandEvent& event );
   void UpdateAxesLabels( wxCommandEvent& event );
   void UpdateScalarBar( wxCommandEvent& event );
   void UpdateMinSlider( wxCommandEvent& event );
   void UpdateMaxSlider( wxCommandEvent& event );

   ///update the base info for the dataset ie. active vector,scalar,dataset,range 
   void _updateBaseInformation();
   ///Update the active dataset
   void _OnSelectDataset(wxCommandEvent& event);
   ///Update the active scalar
   void _OnSelectScalar(wxCommandEvent& event);
   ///Update the active vector
   void _OnSelectVector(wxCommandEvent& event);
   ///Callback for the clear all button
   void OnClearAll( wxCommandEvent& event );
   ///Callback for min spinner
   void _onMinSpinCtrl( wxScrollEvent& WXUNUSED(event) );
   ///Callback for max spinner
   void _onMaxSpinCtrl( wxScrollEvent& WXUNUSED(event) );
   ///Callback for slider max and min values
   void _onMinMaxSlider( wxScrollEvent& WXUNUSED(event) ); 
   ///Callback for slider max and min values
   void _onMinSlider( wxScrollEvent& WXUNUSED(event) );
   ///Callback for slider max and min values
   void _onMaxSlider( wxScrollEvent& WXUNUSED(event) );
   ///Callback to keep sliders from crossing
   bool _ensureSliders(int activeSliderID);
   ///Update the available solutions for a particular give dataset type
   ///\param newNames The list of new names to update
   void _updateAvailableScalarMeshSolutions(VjObs::Scalars newNames);
   ///Selects first scalar and vector if they exist
   void InitialScalarVector();

   void _onClose( wxCommandEvent& event );
   ///Update the available solutions for a particular give dataset type
   ///\param dataType The data set type\n 
   ///Valid types include:\n
   ///MESH_SCALARS == 3d mesh scalar data\n
   ///MESH_VECTORS == 3d mesh vector data\n
   ///TEXTURE_SCALARS == 3d texture scalar data\n
   ///TEXTURE_VECTORS == 3d texture vector data\n
   ///\param newNames The list of new names to update
   void _updateAvailableSolutions(std::string dataType,VjObs::scalar_p newNames);

   ///Update the combo box values a particular give dataset type
   ///\param dataType The data set type\n 
   ///Valid types include:\n
   ///MESH_SCALARS == 3d mesh scalar data\n
   ///MESH_VECTORS == 3d mesh vector data\n
   ///TEXTURE_SCALARS == 3d texture scalar data\n
   ///TEXTURE_VECTORS == 3d texture vector data\n
   ///\param newNames The list of new names to update
   void _updateComboBoxNames(std::string dataType, wxArrayString listOfNames);

   ///Set the the active dataset internally
   ///\param index The index of the dataset to make active.
   void _setActiveDataset(unsigned int index);

   ///Update the dialog information from a model
   ///\param newModel The model to extract new information from.
   void _updateModelInformation(VjObs::Model_var newModel);

   ///Update the dialog information from a dataset
   ///\param datasetInfo The dataset to extract the new information from.
   void _updateDatasetInformation(VjObs::Dataset datasetInfo );
   
   Contours* scalarContour;///<Scalar dialog
   Contours* vectorContour;///<Vector dialog
   Streamlines* streamline;///<Streamline dialog
   Isosurfaces* isosurface;///<Iso-Surface dialog
   TextureBasedToolBar* _tbTools;///<TextureBasedToolBar.
   Polydata* polydata;///<Polydata dialog
   Vistab* vistab;

   unsigned int _nDatasetsInActiveModel;///<The number of datasets in the active model.
   unsigned int _nScalarsInActiveDataset;///<Number of scalars in the active dataset.
   unsigned int _nVectorsInActiveDataset;///<Number of vectors in the active dataset.
   unsigned int _nScalarTexturesInActiveDataset;///<Number of scalars in the active texture dataset.
   unsigned int _nVectorTexturesInActiveDataset;///<Number of vectors in the active texture dataset.

   VjObs::Model_var _activeModel;///<The active Model data from CORBA.
   VjObs::Dataset _activeDataset;///The active Dataset.

   std::map<std::string,wxArrayString> _availableSolutions;///<The current solutions available in the current dataset;
   wxArrayString _availableDatasets;///<The current datasets available in the Model;

   wxRect _vistabPosition;///<The bounding box of the vistab dialog.
   
   wxButton* clearAllButton;///<The clear all button for vis objects
   wxButton* updateAxes;///<The clear all button for vis objects

   wxComboBox* _datasetSelection;///<The box listing the available datasets.
   wxListBox* _scalarSelection;///<The box listing the available scalars in the current dataset.
   wxListBox* _vectorSelection;///<The box listing the available vectors in the current dataset.
   wxCheckBox* wireFrameCB;///<The checkbox to turn wireframe on/off
   wxCheckBox* bboxCB;///<The checkbox to turn bounding box on/off
   wxCheckBox* axesCB;///<The checkbox to turn axes on/off
   wxCheckBox* scalarBarCB;///<The checkbox to turn scalar bar on/off
   wxTextCtrl* xAxisEntry;///< The text entry for axes labels
   wxTextCtrl* yAxisEntry;///< The text entry for axes labels
   wxTextCtrl* zAxisEntry;///< The text entry for axes labels

   std::string _activeScalarName;///<The selected scalar
   std::string _activeVectorName;///<The selected vector
   std::string _activeDataSetName;///<Thea active dataset's name

   std::map<std::string,std::vector<double> > _originalScalarRanges;///<The scalar range for the active scalar
   std::vector<double> _activeScalarRange;///<The active scalars range.

   std::vector<VE_XML::DataValuePair*> _vistabBaseInformation;///<The basic information from the vistab
   std::vector<VE_XML::DataValuePair*> _vistabSpecificInformation;///<The specific information from specific vistab dialogs

   std::string _commandName;///<The name of the command to send back

   std::vector< VE_XML::Command* > commands;
   //VjObs_ptr xplorerPtr;
   std::vector< long > commandInputs;
   //DOMDocument* doc;
   //VE_XML::DOMDocumentManager* domManager;
   std::string dataValueName;

   double minimumValue;
   double maximumValue;

   //bool scalarSelect;
   //bool vectorSelect;

   //unsigned int scalarValue;
   
   std::string ConvertUnicode( const wxChar* data )
   {
      std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
      return tempStr;
   }   

    DECLARE_EVENT_TABLE()
};

#endif
    // _VE-SUITE_H_
