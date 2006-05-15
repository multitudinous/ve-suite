/////////////////////////////////////////////////////////////////////////////
// Name:        vistab.h
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     17/04/2006 16:26:41
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

// Generated by DialogBlocks, 17/04/2006 16:26:41

#ifndef _VISTAB_H_
#define _VISTAB_H_


#include "wx/toolbar.h"
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include "VE_Conductor/Framework/vectors.h"
#include "VE_Conductor/Framework/contours.h"
#include "VE_Conductor/Framework/streamlines.h"
#include "VE_Conductor/Framework/isosurfaces.h"
#include "VE_Conductor/Utilities/DualSlider.h"
#include <xercesc/dom/DOM.hpp>
#include <vector>
XERCES_CPP_NAMESPACE_USE


//class DualSlider;


namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}
namespace VE_Model
{
   class Model;
}

class TextureBasedToolBar;
class wxComboBox;
class wxListBox;

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

enum VISTAB_IDS
{
   CONTOUR_BUTTON,
   VECTOR_BUTTON,
   STREAMLINE_BUTTON,
   ISOSURFACE_BUTTON,
   TEXTURE_BASED_BUTTON
};
////@end control identifiers


#ifndef wxCLOSE_BOX
#define wxCLOSE_BOX 0x1000
#endif



class Vistab: public wxDialog
{    
    DECLARE_EVENT_TABLE()

public:
   ///Constructor
   ///\param activeModel The active Model information from CORBA.
   Vistab(VjObs::Model* activeModel );

   ///Constructor
   ///\param activeModel The active Model information from CORBA.
   Vistab(VjObs::Model* activeModel,
            wxWindow* parent,
            wxWindowID id = SYMBOL_VISTAB_IDNAME,
            const wxString& caption = SYMBOL_VISTAB_TITLE,
            const wxPoint& pos = SYMBOL_VISTAB_POSITION,
            const wxSize& size = SYMBOL_VISTAB_SIZE,
            long style = SYMBOL_VISTAB_STYLE );

    void SendCommandsToXplorer( void );
    void SetCommInstance( VjObs_ptr veEngine );
    /// Creation
    bool Create( wxWindow* parent, wxWindowID id = SYMBOL_VISTAB_IDNAME, const wxString& caption = SYMBOL_VISTAB_TITLE, const wxPoint& pos = SYMBOL_VISTAB_POSITION, const wxSize& size = SYMBOL_VISTAB_SIZE, long style = SYMBOL_VISTAB_STYLE );

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
   void SetActiveModel(VjObs::Model* activeModel);

   ///Set the active dataset
   ///\param name The name of the dataset
   void SetActiveDataset(std::string name);


   Vectors* vector;
   Contours* contour;
   Streamlines* streamline;
   Isosurfaces* isosurface;
   TextureBasedToolBar* _tbTools;///<TextureBasedToolBar.
   VE_Conductor::GUI_Utilities::DualSlider* scalarRange;

   wxToolBar*  itemToolBar3;
   wxComboBox* itemComboBox11;
   wxComboBox* itemComboBox12; 
   wxListBox*  itemListBox13; 
   wxListBox*  itemListBox15;

protected:
   void _onContour(wxCommandEvent& );
   void _onVector(wxCommandEvent& );
   void _onStreamline(wxCommandEvent& );
   void _onIsosurface(wxCommandEvent& );
   void _onTextureBased(wxCommandEvent& );

   ///Update the available solutions for a particular give dataset type
   ///\param newNames The list of new names to update
   void _updateAvailableScalarMeshSolutions(VjObs::Scalars newNames);

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
   void _updateModelInformation(VjObs::Model* newModel);

   ///Update the dialog information from a dataset
   ///\param datasetInfo The dataset to extract the new information from.
   void _updateDatasetInformation(VjObs::Dataset datasetInfo );

   
   unsigned int _nDatasetsInActiveModel;///<The number of datasets in the active model.
   unsigned int _nScalarsInActiveDataset;///<Number of scalars in the active dataset.
   unsigned int _nVectorsInActiveDataset;///<Number of vectors in the active dataset.
   unsigned int _nScalarTexturesInActiveDataset;///<Number of scalars in the active texture dataset.
   unsigned int _nVectorTexturesInActiveDataset;///<Number of vectors in the active texture dataset.

   VjObs::Model* _activeModel;///<The active Model data from CORBA.
   VjObs::Dataset _activeDataset;///The active Dataset.

   std::map<std::string,wxArrayString> _availableSolutions;///<The current solutions available in the Model;

   wxComboBox* _scalarSelection;///<The box listing the available attributes.
   wxComboBox* _vectorSelection;///<The box listing the available attributes.


   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   std::vector< long > commandInputs;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
   std::string dataValueName;
};

#endif
    // _VE-SUITE_H_
