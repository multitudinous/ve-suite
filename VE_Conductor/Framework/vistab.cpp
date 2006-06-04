/////////////////////////////////////////////////////////////////////////////
// Name:        vistab.cpp
// Purpose:     
// Author:      Jared Abodeely
// Modified by: 
// Created:     17/04/2006 16:26:41
// RCS-ID:      
// Copyright:   
// Licence:     
/////////////////////////////////////////////////////////////////////////////

// Generated by DialogBlocks (unregistered), 17/04/2006 16:26:41


////@begin includes
#include "VE_Conductor/Framework/TBToolBar.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
////@end includes

#include "VE_Conductor/Framework/vistab.h"
#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>
#include <wx/radiobut.h>
#include <wx/slider.h>
#include <wx/icon.h>
#include <wx/statbmp.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/msgdlg.h>

#include <iostream>
#include <string>
////@begin XPM images
#include "VE_Conductor/Framework/vector.xpm"
#include "VE_Conductor/Framework/contour.xpm"
////@end XPM images

BEGIN_EVENT_TABLE( Vistab, wxDialog )
////@begin Vistab event table entries
   EVT_TOOL     (CONTOUR_BUTTON,       Vistab::_onContour)
   EVT_TOOL     (VECTOR_BUTTON,        Vistab::_onVector)
   EVT_TOOL     (STREAMLINE_BUTTON,    Vistab::_onStreamline)
   EVT_TOOL     (ISOSURFACE_BUTTON,    Vistab::_onIsosurface)
   EVT_TOOL     (TEXTURE_BASED_BUTTON, Vistab::_onTextureBased)
   EVT_COMBOBOX(ID_COMBOBOX,Vistab::_OnSelectDataset)
   EVT_LISTBOX(ID_LISTBOX,Vistab::_OnSelectScalar)
   EVT_LISTBOX(ID_LISTBOX1,Vistab::_OnSelectVector)
////@end Vistab event table entries
END_EVENT_TABLE()
using namespace VE_Conductor::GUI_Utilities;

//////////////////////////////////////////
//Constructor                           //
//////////////////////////////////////////
Vistab::Vistab(VjObs::Model_var activeModel )
{
   _activeModel = 0;
   _scalarSelection = 0;    
   _vectorSelection = 0;    
   _nDatasetsInActiveModel = 0;
   _datasetSelection = 0;
   _tbTools = 0;
   xplorerPtr = 0;

   _availableSolutions["MESH_SCALARS"].Add(""); 
   _availableSolutions["MESH_VECTORS"].Add(""); 
   _availableSolutions["TEXTURE_SCALARS"].Add("");  
   _availableSolutions["TEXTURE_VECTORS"].Add("");
   _commandName = "VISUALIZATION_TAB";
   SetActiveModel(activeModel);
   _activeScalarName = _scalarSelection->GetStringSelection();
   _activeVectorName = _vectorSelection->GetStringSelection();
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
}
///////////////////////////////////////////////////////////////////
//Constructor                                                    //
///////////////////////////////////////////////////////////////////
Vistab::Vistab(VjObs::Model_var activeModel,
               wxWindow* parent, wxWindowID id,
               const wxString& caption,
               const wxPoint& pos, const wxSize& size, long style )
{
   _activeModel = 0;
   _scalarSelection = 0;    
   _vectorSelection = 0;    
   _nDatasetsInActiveModel = 0;
   _datasetSelection = 0;
   xplorerPtr = 0;
   
   _availableSolutions["MESH_SCALARS"].Add(""); 
   _availableSolutions["MESH_VECTORS"].Add(""); 
   _availableSolutions["TEXTURE_SCALARS"].Add("");  
   _availableSolutions["TEXTURE_VECTORS"].Add(""); 

   SetActiveModel(activeModel);
   Create(parent, id, caption, pos, size, style);
   if(_nDatasetsInActiveModel)
   {
      _setActiveDataset(0);
   }
   _commandName = "VISUALIZATION_TAB";
   _activeScalarName = _scalarSelection->GetStringSelection();
   _activeVectorName = _vectorSelection->GetStringSelection();
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
}
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
bool Vistab::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
////@begin Vistab member initialisation
   itemToolBar3 = 0;
   itemComboBox11 = 0;
   itemComboBox12 = 0;
   itemListBox13 = 0; 
   itemListBox15 = 0;
   vector = 0;
   contour = 0;
   streamline = 0;
   isosurface = 0;
   _tbTools = 0;
   scalarRange = 0;
////@end Vistab member initialisation

////@begin Vistab creation
    SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
    wxDialog::Create( parent, id, caption, pos, size, style );

    CreateControls();
    GetSizer()->Fit(this);
    GetSizer()->SetSizeHints(this);
    Centre();
//    GetSizer()->SetDimension(427, 20, 427, 400);
////@end Vistab creation
    return true;
}
/////////////////////////////
void Vistab::CreateControls()
{    
////@begin Vistab content construction
    // Generated by DialogBlocks, Wed 26 Apr 2006 18:52:50 CDT 

    Vistab* itemDialog1 = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    itemDialog1->SetSizer(itemBoxSizer2);

    itemToolBar3 = new wxToolBar( itemDialog1, ID_TOOLBAR, wxDefaultPosition, wxDefaultSize, wxTB_HORIZONTAL );
    itemToolBar3->SetToolBitmapSize(wxSize(45, 30));

    wxBitmap itemtool4Bitmap(contour_xpm);
    wxBitmap itemtool4BitmapDisabled;
    itemToolBar3->AddTool(CONTOUR_BUTTON, _T(""), itemtool4Bitmap, itemtool4BitmapDisabled, wxITEM_RADIO, _T("Scalar Contours"), wxEmptyString);

    wxBitmap itemtool5Bitmap(vector_xpm);
    wxBitmap itemtool5BitmapDisabled;
    itemToolBar3->AddTool(VECTOR_BUTTON, _T(""), itemtool5Bitmap, itemtool5BitmapDisabled, wxITEM_RADIO, _T("Vectors"), wxEmptyString);

    wxBitmap itemtool6Bitmap(vector_xpm);
    wxBitmap itemtool6BitmapDisabled;
    itemToolBar3->AddTool(STREAMLINE_BUTTON, _T(""), itemtool6Bitmap, itemtool6BitmapDisabled, wxITEM_RADIO, _T("Streamlines"), wxEmptyString);

    wxBitmap itemtool7Bitmap(vector_xpm);
    wxBitmap itemtool7BitmapDisabled;
    itemToolBar3->AddTool(ISOSURFACE_BUTTON, _T(""), itemtool7Bitmap, itemtool7BitmapDisabled, wxITEM_RADIO, _T("Isosurfaces"), wxEmptyString);

    wxBitmap itemtool8Bitmap(vector_xpm);
    wxBitmap itemtool8BitmapDisabled;
    itemToolBar3->AddTool(TEXTURE_BASED_BUTTON, _T(""), itemtool8Bitmap, itemtool8BitmapDisabled, wxITEM_RADIO, _T("Texture Based"), wxEmptyString);

    itemToolBar3->Realize();
    itemBoxSizer2->Add(itemToolBar3, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

    wxBoxSizer* itemBoxSizer10 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer10, 0, wxGROW|wxALL, 5);

    _datasetSelection = new wxComboBox( itemDialog1, ID_COMBOBOX, _T(""), wxDefaultPosition, wxDefaultSize, _availableDatasets, wxCB_DROPDOWN );
    
    if (ShowToolTips())
        _datasetSelection->SetToolTip(_T("Data Sets"));
    itemBoxSizer10->Add(_datasetSelection, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    /*wxString itemComboBox12Strings[] = {
        _T("DataSet1"),
        _T("DataSet2"),
        _T("DataSet3")
    };
    itemComboBox12 = new wxComboBox( itemDialog1, ID_COMBOBOX1, _T(""), wxDefaultPosition, wxDefaultSize, 3, itemComboBox12Strings, wxCB_DROPDOWN );

    itemBoxSizer10->Add(itemComboBox12, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);*/

    wxBoxSizer* itemBoxSizer11 = new wxBoxSizer(wxHORIZONTAL);
    itemBoxSizer2->Add(itemBoxSizer11, 0, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer12Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Scalars"));
    wxStaticBoxSizer* itemStaticBoxSizer12 = new wxStaticBoxSizer(itemStaticBoxSizer12Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer12, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   _scalarSelection = new wxListBox( itemDialog1, ID_LISTBOX, wxDefaultPosition, wxSize(125, -1), _availableSolutions["MESH_SCALARS"] , wxLB_SINGLE );
   _scalarSelection->SetSelection(0);    
   itemStaticBoxSizer12->Add(_scalarSelection, 1, wxGROW|wxALL, 5);

    wxStaticBox* itemStaticBoxSizer14Static = new wxStaticBox(itemDialog1, wxID_ANY, _T("Vectors"));
    wxStaticBoxSizer* itemStaticBoxSizer14 = new wxStaticBoxSizer(itemStaticBoxSizer14Static, wxHORIZONTAL);
    itemBoxSizer11->Add(itemStaticBoxSizer14, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    _vectorSelection = new wxListBox( itemDialog1, ID_LISTBOX1, wxDefaultPosition, wxSize(125, -1), _availableSolutions["MESH_VECTORS"], wxLB_SINGLE );
    _vectorSelection->SetSelection(0);
    itemStaticBoxSizer14->Add(_vectorSelection, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5);

    wxBoxSizer* scalarSizer = new wxBoxSizer(wxHORIZONTAL);
    scalarRange = new DualSlider(this,-1,1,0,100,0,100,wxDefaultPosition,wxDefaultSize,
                             wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS,wxString("Scalar Bounds"));
    scalarSizer->Add(scalarRange,1,wxALIGN_CENTER|wxEXPAND);
    
    itemBoxSizer2->Add(scalarSizer, 3, wxALIGN_CENTER|wxEXPAND|wxALL, 5);
   
    wxButton* itemButton20 = new wxButton( itemDialog1, ID_BUTTON, _T("Advanced..."), wxDefaultPosition, wxDefaultSize, 0 );
    itemBoxSizer2->Add(itemButton20, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

///@end Vistab content construction
}
///////////////////////////
bool Vistab::ShowToolTips()
{
    return true;
}


//////////////////////////////////////////////////////////
wxBitmap Vistab::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin Vistab bitmap retrieval
  /*  wxUnusedVar(name);
    if (name == _T("../../../../../../../home/users/jaredabo/GUIs/GUI/contour.png"))
    {
        wxBitmap bitmap(_T("../../../../../../../home/users/jaredabo/GUIs/GUI/contour.png"), wxBITMAP_TYPE_PNG);
        return bitmap;
    }
    else if (name == _T("../../../../../../../home/users/jaredabo/GUIs/GUI/vector.png"))
    {
        wxBitmap bitmap(_T("../../../../../../../home/users/jaredabo/GUIs/GUI/vector.png"), wxBITMAP_TYPE_PNG);
        return bitmap;
    }*/
    return wxNullBitmap;
////@end Vistab bitmap retrieval
}

/*!
 * Get icon resources
 */


//////////////////////////////////////////////////////
wxIcon Vistab::GetIconResource( const wxString& name )
{
    // Icon retrieval
    //wxUnusedVar(name);
    return wxNullIcon;
}
//////////////////////////////////////////////////
void Vistab::SetCommInstance( VjObs_ptr veEngine )
{
   xplorerPtr = VjObs::_duplicate( veEngine );
}
////////////////////////////////////////////////////////////
void Vistab::_onContour( wxCommandEvent& WXUNUSED(event) )
{
   contour = new Contours(this,                
                  SYMBOL_CONTOURS_IDNAME, 
                  SYMBOL_CONTOURS_TITLE,
                  SYMBOL_CONTOURS_POSITION,
                  SYMBOL_CONTOURS_SIZE, 
                  SYMBOL_CONTOURS_STYLE );
   contour->ShowModal();
}
/////////////////////////////////////////////////////////
void Vistab::_onVector( wxCommandEvent& WXUNUSED(event) )
{
   vector = new Vectors( this,                
                  SYMBOL_VECTORS_IDNAME, 
                  SYMBOL_VECTORS_TITLE,
                  SYMBOL_VECTORS_POSITION,
                  SYMBOL_VECTORS_SIZE, 
                  SYMBOL_VECTORS_STYLE );
   vector->ShowModal();
}
////////////////////////////////////////////////////////////
void Vistab::_onStreamline( wxCommandEvent& WXUNUSED(event) )
{
   streamline = new Streamlines( this,                
                  SYMBOL_STREAMLINES_IDNAME, 
                  SYMBOL_STREAMLINES_TITLE,
                  SYMBOL_STREAMLINES_POSITION,
                  SYMBOL_STREAMLINES_SIZE, 
                  SYMBOL_STREAMLINES_STYLE );
   streamline->ShowModal();
std::cout<<"STREAMLINES WORKING"<<std::endl;
}
////////////////////////////////////////////////////////////
void Vistab::_onIsosurface( wxCommandEvent& WXUNUSED(event) )
{
   isosurface = new Isosurfaces( this,                
                  SYMBOL_ISOSURFACES_IDNAME, 
                  SYMBOL_ISOSURFACES_TITLE,
                  SYMBOL_ISOSURFACES_POSITION,
                  SYMBOL_ISOSURFACES_SIZE, 
                  SYMBOL_ISOSURFACES_STYLE );
   isosurface->ShowModal();
}
////////////////////////////////////////////////////////////
void Vistab::_onTextureBased( wxCommandEvent& WXUNUSED(event) )
{
   
   TextureBasedToolBar tbTools(this,-1);
   tbTools.SetVjObsPtr(xplorerPtr);
   if(tbTools.ShowModal() == wxID_OK)
   {
      std::cout<<"TBTools WORKING"<<std::endl;
   }
}
///////////////////////////////////////////////////////
void Vistab::SetActiveModel(VjObs::Model_var activeModel)
{
   _activeModel = activeModel;
   _updateModelInformation(_activeModel);
}
///////////////////////////////////////////////
void Vistab::SetActiveDataset(std::string name)
{
   //is this incorrect to assume we have a model? -- biv
   if(_activeModel)
   {
      //loop over available datasets
      for(unsigned int i = 0; i < _activeModel->dataVector.length(); i++)
      {
         if(wxString(_activeModel->dataVector[i].datasetname) == wxString(name.c_str()))
         {
            _setActiveDataset(i);
            return;
         }
      }
   }
}
//////////////////////////////////////////////////////////////
void Vistab::_updateModelInformation(VjObs::Model_var newModel)
{
   //number of datasets
   _nDatasetsInActiveModel =  newModel->dataVector.length();   
   if(_nDatasetsInActiveModel > 0 )
   {
   std::cout<<"nDatasetsInActiveModel: "<< _nDatasetsInActiveModel<<std::endl;
      //get all the dataset names in this model
      _availableDatasets.Clear();
      for(unsigned int i = 0; i < _nDatasetsInActiveModel; i++)
      {
         std::cout<<"Dataset name: "<< newModel->dataVector[i].datasetname<<std::endl;
         _availableDatasets.Add(wxString(newModel->dataVector[i].datasetname));
      }

      //set the active dataset to the initial dataset in the model
      _setActiveDataset(0); 
   }
}
//////////////////////////////////////////////////
void Vistab::_setActiveDataset(unsigned int index)
{
   std::cout<<"setActiveDataset"<<std::endl;
   //is this incorrect to assume we have a model? -- biv
   if(_activeModel)
   {
      CORBA::ULong i = index;
      if(i < _nDatasetsInActiveModel)
      {
         _activeDataset = _activeModel->dataVector[i];
         _activeDataSetName = _activeDataset.datasetname;
         if(_datasetSelection)
         {
            _datasetSelection->SetSelection(i);
         }
         //update the available scalar, vector and texture data
         _updateDatasetInformation(_activeDataset);
      }
   }
}
////////////////////////////////////////////////////////////////////
void Vistab::_updateDatasetInformation(VjObs::Dataset datasetInfo )
{
   std::cout<<"udpateDatasetInformation"<<std::endl;
   _nScalarsInActiveDataset = datasetInfo.scalarVector.length();
   if(_nScalarsInActiveDataset)
   {
      _updateAvailableScalarMeshSolutions(datasetInfo.scalarVector);
   }

   _nVectorsInActiveDataset = datasetInfo.vectornames.length();
   if(_nVectorsInActiveDataset)
   {
      _updateAvailableSolutions("MESH_VECTORS",datasetInfo.vectornames);
   }
     
   _nScalarTexturesInActiveDataset = datasetInfo.textureScalarNames.length();
   if(_nScalarTexturesInActiveDataset)
   {
      _updateAvailableSolutions("TEXTURE_SCALARS",datasetInfo.textureScalarNames);
   }

   _nVectorTexturesInActiveDataset = datasetInfo.textureVectorNames.length();
   if(_nVectorTexturesInActiveDataset)
   {
      _updateAvailableSolutions("TEXTURE_VECTORS",datasetInfo.textureVectorNames);
   }
}
////////////////////////////////////////////////////////////////////////
//We need this extra method because the data info is stored differenly//
//for scalars                                                         // 
////////////////////////////////////////////////////////////////////////
void Vistab::_updateAvailableScalarMeshSolutions(VjObs::Scalars newScalars)
{
   ///clear out the scalar ranges from before
   _originalScalarRanges.clear();

   std::cout<<"udpateAvailableScalarMeshSolutions"<<std::endl;
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( "MESH_SCALARS" );
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < newScalars.length(); i++)
      {
         _originalScalarRanges[std::string(newScalars[i].scalarnames)].push_back(newScalars[i].scalarrange[0]);
         _originalScalarRanges[std::string(newScalars[i].scalarnames)].push_back(newScalars[i].scalarrange[1]);

         currentSolution->second.Add(wxString(newScalars[i].scalarnames));
      }
   }
   else
   {
      std::cout<<"MESH_SCALAR data not available in current dataset: "<<_activeDataset.datasetname<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////////
void Vistab::_updateAvailableSolutions(std::string dataType,
                                       VjObs::scalar_p names)
{
   std::cout<<"udpateAvailableSolutions"<<std::endl;
   std::map<std::string, wxArrayString >::iterator currentSolution;
   currentSolution = _availableSolutions.find( dataType );
   if(currentSolution != _availableSolutions.end())
   {
      currentSolution->second.Clear();
      for(size_t i = 0; i < names.length(); i++)
      {
         currentSolution->second.Add(wxString(names[i]));
      }
   }
   else
   {
      std::cout<<dataType<<" not available in current dataset: "<<_activeDataset.datasetname<<std::endl;
   }
}
///////////////////////////////////////////////////////////
void Vistab::_updateComboBoxNames(std::string dataType,
                                 wxArrayString listOfNames)
{
   std::cout<<"updateComboBoxNames"<<std::endl;
   wxListBox* activeComboBox = 0; 
   if(dataType == "MESH_SCALARS" ||
      dataType == "TEXTURE_SCALARS")
   {
      activeComboBox = _scalarSelection;
   }
   else if(dataType == "MESH_VECTORS" ||
         dataType == "TEXTURE_VECTORS")
   {
      activeComboBox = _vectorSelection;
   }
   activeComboBox->Clear();
   for(size_t i = 0; i < listOfNames.GetCount(); i++)
   {
      activeComboBox->Insert(listOfNames[i],i);
   }
}
////////////////////////////////////////////
DualSlider* Vistab::GetScalarRangeControls()
{
   return scalarRange;
}
////////////////////////////////////////////////////
void Vistab::_OnSelectDataset(wxCommandEvent& event)
{
   _activeDataSetName = _datasetSelection->GetValue();
}
///////////////////////////////////////////////////
void Vistab::_OnSelectScalar(wxCommandEvent& event)
{
   _activeScalarName = _scalarSelection->GetStringSelection();
   _activeScalarRange = _originalScalarRanges[_activeScalarName];
}
///////////////////////////////////////////////////
void Vistab::_OnSelectVector(wxCommandEvent& event)
{
   _activeVectorName = _vectorSelection->GetStringSelection();
}
//////////////////////////////////////////
std::string Vistab::GetActiveScalarName()
{
   return _activeScalarName;
}
//////////////////////////////////////////
std::string Vistab::GetActiveVectorName()
{
   return _activeVectorName;
}
//////////////////////////////////////////
std::string Vistab::GetActiveDatasetName()
{
   return _activeDataSetName;
}
////////////////////////////////////
void Vistab::ClearBaseInformation()
{
   _vistabBaseInformation.clear();
   _commandName.clear();
}
////////////////////////////////////////
void Vistab::ClearSpecificInformation()
{
   _vistabSpecificInformation.clear();
   _commandName.clear();
}
///////////////////////////////////////////
void Vistab::_updateBaseInformation()
{
   ///This is the default. Other dialogs actions will set the command name to the specific value if they are launched
   _commandName = "VISUALIZATION_TAB";

   VE_XML::DataValuePair* activeScalar = new VE_XML::DataValuePair();
   activeScalar->SetDataType("STRING");
   activeScalar->SetDataName(std::string("Active Scalar"));
   activeScalar->SetDataString(_activeScalarName);

   _vistabBaseInformation.push_back(activeScalar);
   
   VE_XML::DataValuePair* activeVector = new VE_XML::DataValuePair();
   activeVector->SetDataType("STRING");
   activeVector->SetDataName(std::string("Active Vector"));
   activeVector->SetDataString(_activeVectorName);

   _vistabBaseInformation.push_back(activeVector);

   VE_XML::DataValuePair* activeDataset= new VE_XML::DataValuePair();
   activeDataset->SetDataType("STRING");
   activeDataset->SetDataName(std::string("Active Dataset"));
   activeDataset->SetDataString(_activeDataSetName);

   _vistabBaseInformation.push_back(activeDataset);

   VE_XML::DataValuePair* scalarMin = new VE_XML::DataValuePair();
   double minimumValue = _activeScalarRange.at(0) 
                    + (scalarRange->GetMinSliderValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   scalarMin->SetData("Scalar Min",minimumValue);

   _vistabBaseInformation.push_back(scalarMin);

   VE_XML::DataValuePair* scalarMax = new VE_XML::DataValuePair();
   double maximumValue = _activeScalarRange.at(0) 
                    + (scalarRange->GetMaxSliderValue()/100.0)*(_activeScalarRange.at(1) - _activeScalarRange.at(0));
   scalarMax->SetData("Scalar Max",maximumValue);
   
   _vistabBaseInformation.push_back(scalarMax);
}
////////////////////////////////////////////////////////////////////////////
void Vistab::SendUpdatedSettingsToXplorer(VE_XML::Command* subDialogCommand)
{
   _updateBaseInformation();
   VE_XML::Command* newCommand = new VE_XML::Command();

   for(size_t i =0; i < _vistabBaseInformation.size(); i++)
   {
      newCommand->AddDataValuePair(_vistabBaseInformation.at(i));
   }
   if(subDialogCommand)
   {
      VE_XML::DataValuePair* subDialogSettings = new VE_XML::DataValuePair();
      subDialogSettings->SetData("Sub-Dialog Settings",subDialogCommand);
      newCommand->AddDataValuePair(subDialogSettings);
   }

   newCommand->SetCommandName(_commandName);

   std::string commandString("returnString");

   VE_XML::XMLReaderWriter commandWriter;
   commandWriter.UseStandaloneDOMDocumentManager();
   commandWriter.WriteToString();
   
   std::pair<VE_XML::Command*,std::string> nodeTagPair;
   nodeTagPair.first = newCommand;
   nodeTagPair.second = std::string("vecommand");
   std::vector< std::pair<VE_XML::XMLObject*,std::string> > nodeToWrite;
   nodeToWrite.push_back(nodeTagPair);

   commandWriter.WriteXMLDocument(nodeToWrite,commandString,"Command");

   char* tempDoc = new char[ commandString.size() + 1 ];
   tempDoc = CORBA::string_dup( commandString.c_str() );

   if ( !CORBA::is_nil( xplorerPtr ) && !commandString.empty() )
   {
      try
      {
         //std::cout<<"---The command to send---"<<std::endl;
         //std::cout<<tempDoc<<std::endl;
         // CORBA releases the allocated memory so we do not have to
         xplorerPtr->SetCommandString( tempDoc );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                        "Communication Failure", wxOK | wxICON_INFORMATION );
         delete [] tempDoc;
      }
   }
   else
   {
      delete [] tempDoc;
   }
   //Clean up memory
   delete newCommand;
}
