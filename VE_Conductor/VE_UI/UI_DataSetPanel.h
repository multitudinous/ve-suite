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
 * File:          $RCSfile: UI_DataSetPanel.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_UI_DATASET_PANEL_H_
#define _VE_UI_DATASET_PANEL_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/scrolwin.h>

class wxString;
class wxRadioBox;
class wxBoxSizer;
class wxStaticBoxSizer;
class wxSizer;
class wxSlider;
class wxComboBox;
class wxStaticText;
class wxStaticBox;
class wxButton;

#include <vector>
#ifdef _TAO
#include "VjObsC.h"
#else
#include "VjObs.h"
#endif

//using namespace std;
class wxSpinCtrlDbl;
class UI_ModelData;

enum DATASETS_PANEL_IDS{
   DATA_SET_SELECT_COMBO,
   DATASET_POLYDATA_RBOX,
   RBOX_3D,
   VERTEX_RBOX,
   POLYDATA_RBOX,
   SCALAR_PANEL_RAD_BOX,
   SCALAR_PANEL_UPDATE_BUTTON,
   MIN_PER_SLIDER_PANEL,
   MAX_PER_SLIDER_PANEL,
   VECTOR_PANEL_RAD_BOX,
   MIN_SPIN_CNTL_BOX,
   MAX_SPIN_CNTL_BOX
};

class UI_Scalars{
public:
   UI_Scalars(wxString*);
   ~UI_Scalars();

   wxString _thisScalarName;
   double range[ 2 ];
   double lastMinSetting;
   double lastMaxSetting;
};

class UI_DataSets{
public:
   UI_DataSets();
   ~UI_DataSets();

   UI_Scalars* thisScalar;
   UI_Scalars* thisVector;
   std::vector<UI_Scalars*> _Scalars;
   std::vector<UI_Scalars*> _Vectors;

   void _buildScalars(int, wxString*, std::vector< std::pair<double, double> > );
   void _buildVectors(int, wxString* );

   wxString _dataSetName;
   int _dataSetType;
   int _numOfScalars;
   int _numOfVectors;
};



class UI_DatasetScroll: public wxScrolledWindow{
public:
   UI_DatasetScroll(wxWindow* parent);
   ~UI_DatasetScroll();

   void changeActiveDatasetType( int );

   wxRadioBox* _3dRBox;
   //wxRadioBox* _vertexRBox;
   //wxRadioBox* _polydataRBox;
   wxBoxSizer* _col;

   DECLARE_EVENT_TABLE()
};


class UI_ScalarScroll: public wxScrolledWindow{
public:
   UI_ScalarScroll(wxWindow* parent);
   ~UI_ScalarScroll();

   wxRadioBox* _scalarRBox;
   wxRadioBox* _vectorRBox;
   wxBoxSizer* _col;

   void rebuildRBoxes(UI_DataSets*);

   DECLARE_EVENT_TABLE()
};


class UI_DatasetPanel: public wxPanel{
public:
   //UI_DatasetPanel(wxWindow* tControl);
   UI_DatasetPanel(wxWindow* tControl, UI_ModelData* _model, int activeMod);
   ~UI_DatasetPanel();

   UI_DataSets* thisDataSet;
   //UI_DataSets* activeDataSet;

   int _numSteadyStateDataSets;

   std::vector<UI_DataSets*> _DataSets;

   UI_DatasetScroll* _RBoxScroll;
   UI_ScalarScroll* _ScalarScroll;
  
   UI_ModelData* _modelData;
   int _activeModIndex;

   int _maxnoScalars;
   int _noScalars;

   wxString* _scalarNames;
   wxString* _vectorNames;

   int _no3DMesh;
   int _noVertex;
   int _noPolydata;
   wxString* meshArrayNames;  
   wxString* vertexArrayNames;
   wxString* polydataArrayNames;
   wxString datatypes[3];

   //Building the panel
   void _buildPanel();

   wxComboBox* _datasetCombo;
   wxString _datasetTypesel[3];

   //the controls
   //wxRadioBox* _activeRBox;

   wxStaticText* minLabel;
   wxStaticText* maxLabel;

   wxBoxSizer* minGroup; 
   wxBoxSizer* maxGroup; 
   
   wxButton* _visUpdateButton;

   wxStaticBox* _scalarRangeBox;
   wxSlider* _maxPercentSlider;
   wxSlider* _minPercentSlider;
   wxSpinCtrlDbl* _minSpinner;
   wxSpinCtrlDbl* _maxSpinner;

   wxBoxSizer* scalgroupmin;
   wxBoxSizer* scalgroupspacer;
   wxBoxSizer* scalgroupmax;

   wxBoxSizer* minGroupwspin;
   wxBoxSizer* maxGroupwspin;

   wxStaticBox* _dataHeadingBox;

   wxBoxSizer* _top;
   wxBoxSizer* _bottom;

   wxStaticBoxSizer* sRangeBoxSizer;

   wxStaticBoxSizer* dHeadingBoxSizer;

   wxSizer* _3dSizer;

   wxBoxSizer* _colcombine1_2;

   wxBoxSizer* _mastercol1;

   wxBoxSizer* _col1;
   wxBoxSizer* _col2;
   wxBoxSizer* _col3;
   wxBoxSizer* _col4;

   wxBoxSizer* datasetPanelGroup;

   /*VjObs::scalar_p_var datasetNames;
   VjObs::obj_p_var datasetTypes;
   VjObs::obj_p_var numScalarsPerDataset;
   VjObs::scalar_p_var scalarNames;*/

   void _setScalars( UI_DataSets* );
   void _rebuildDataSets( int );
protected:
   void _buildDataSets();
   
   void _setScalarsnoDatasets();
   void _organizeRadioBoxInfo();
   void _organizeActiveRBox();
   void _resetScalarAdjustment( int, int );
   void _onActiveSelection(wxCommandEvent& event);
   void _on3d(wxCommandEvent& event);
   void _onVertex(wxCommandEvent& event);
   void _onPolyData(wxCommandEvent& event);
   void _onScalars(wxCommandEvent& event);
   void _onVectors(wxCommandEvent& event);
   void _onUpdate(wxCommandEvent& event);
   void _onMinMaxSlider(wxScrollEvent& event);
   void _onMinSpinCtrl(wxScrollEvent& event);
   void _onMaxSpinCtrl(wxScrollEvent& event);

   void GetMinMaxScalar( double&, double& );
   void ConstructCommandId( void );
   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_DATASET_PANEL_H_



