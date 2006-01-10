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
 * File:          $RCSfile: tcFrame.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _TC_FRAME_H_
#define _TC_FRAME_H_
#include <wx/wx.h>
#include <wx/spinctrl.h>
#include <wx/progdlg.h>
#include "textureCreator.h"

#include <vector>
#include <string>

enum TCFrameIDs{
   TRANSLATE_BUTTON,
   INPUT_TEXT_BOX,
   OUTPUT_TEXT_BOX,
   INPUT_BROWSE,
   OUTPUT_BROWSE,
   XRES_BOX,
   YRES_BOX,
   ZRES_BOX,
   NUM_FILES,
   GRID_RBOX,
   QUIT_BUTTON
};
class TCFrame: public wxFrame{
public:
   TCFrame (wxWindow* parent,
           wxWindowID id, 
           const wxString& title,
           const wxPoint& pos = wxDefaultPosition, 
           const wxSize& size = wxDefaultSize, 
           long style = wxDEFAULT_FRAME_STYLE);

   virtual ~TCFrame();
   enum GridType{STRUCTURED,UNSTRUCTURED,RECTILINEAR};
   void UpdateStatus(const char* statusString);
   void SetGridType(GridType type);
   void UpdateProgressDialog(const char* msg);

   unsigned int GetNumberOfTimeSteps(){return _numFiles;}

   ///////////////////////////////
   //Batch translation interface//
   ///////////////////////////////
   void SetInputDirectory(const char* inDirectory);
   void SetOutputDirectory(const char* outDirectory);
   void SetTextureResolution(int x, int y, int z);
   void BatchTranslation();

   void SetMPIVariables( int, int );
   
protected:
   GridType _type;
   void _buildGUI();
   int _numFiles;
   int _resolution[3];
   int _currentFile;
   std::vector<const char*> _inputFiles;
   wxString _inputDir;
   wxString _outputDir;

   wxString _outputTextFile;

   wxDirDialog* _dirDialog;
   
   wxArrayString _gridFiles;
   std::vector< std::string > gridFiles;

   wxButton* _browseInputDir;
   wxButton* _browseOutputDir;
   wxButton* _goButton;
   wxButton* _quitButton;

   wxGauge* _transProgress;
   wxProgressDialog* _fileProgress;

   wxTextCtrl* _inputDirBox;
   wxTextCtrl* _outputDirBox;
   wxSpinCtrl* _numFilesBox;

   wxComboBox* _xResBox;
   wxComboBox* _yResBox;
   wxComboBox* _zResBox;

   wxRadioBox* _gridTypeBox;

   VTKDataToTexture* _translator;

   
   //event callbacks
   void _onQuitCallback(wxCommandEvent& event);
   void _onTranslateCallback(wxCommandEvent& event);
   void _onBrowseCallback(wxCommandEvent& event);
   void _chooseDirectory(int style, int browseID);
   void _onResolutionCallback(wxCommandEvent& event);
   void _onNumFilesCallback(wxSpinEvent& event);
   void _onGridTypeCallback(wxCommandEvent& event);
private:
   int numProcessors;
   int rank;

   DECLARE_EVENT_TABLE()
};
#endif//_TC_FRAME_H_
