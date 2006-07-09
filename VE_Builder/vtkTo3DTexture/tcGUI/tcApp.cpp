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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "tcApp.h"
#include <wx/cmdline.h>
#include <cassert>
#ifdef USE_MPI
#include <mpi.h>
#endif
IMPLEMENT_APP(TCApp)

//////////////////////////////
//Initialize the application//
//////////////////////////////
bool TCApp::OnInit()
{
   wxApp::OnInit();
   int ierror;
   p = 1;
   rank = 0;
  /* // Initialize MPI
   ierror = MPI_Init( &(wxGetApp().argc), &(wxGetApp().argv) );
   ierror = MPI_Comm_size( MPI_COMM_WORLD, &p  );
   ierror = MPI_Comm_rank( MPI_COMM_WORLD, &rank );
   */
   //Create the main window
   if(_isBatch){
      return (_translateFromCmdLine()); 
   }else{
       _frame = new TCFrame(0,-1,wxT("Texture Creator"));
      // Problem with generic wxNotebook implementation whereby it doesn't size
      // properly unless you set the size again
#if defined(__WIN16__) || defined(__WXMOTIF__)
       int width, height;
       _frame->GetSize(& width, & height);
       _frame->SetSize(-1, -1, width, height);
#endif
      _frame->SetMPIVariables( rank, p );
      //display the UI
      _frame->Show();
      return TRUE;
   }
}
////////////////////////////////////
bool TCApp::_translateFromCmdLine()
{
   _frame->BatchTranslation();
   return false;
}
//////////////////////////////////////////////////
void TCApp::OnInitCmdLine(wxCmdLineParser& parser)
{
   if(argc < 2){
      _isBatch = false;
      return;
   }
   _isBatch = true;
   p = 1;
   rank = 0;
#ifdef USE_MPI
   // Initialize MPI
   int ierror;
   ierror = MPI_Init( &(wxGetApp().argc), &(wxGetApp().argv) );
   ierror = MPI_Comm_size( MPI_COMM_WORLD, &p  );
   ierror = MPI_Comm_rank( MPI_COMM_WORLD, &rank );
#endif
   static const wxCmdLineEntryDesc cmdLineOpts[] = 
   {
      { wxCMD_LINE_SWITCH, _T("v"), _T("verbose"), _T("be verbose") },
      { wxCMD_LINE_SWITCH, _T("q"), _T("quiet"),   _T("be quiet") },
      { wxCMD_LINE_OPTION,
        _T("o"), _T("odir"),
        _T("Directory to write results,defaults to input directory. Default is current dir.")},
      
      { wxCMD_LINE_OPTION,
        _T("g"), _T("gridType"),
        _T("VTK input dataset grid type\n r == Rectilinear\n s == Structured\n u ==Unstructured")},
      
      { wxCMD_LINE_OPTION,
        _T("x"), _T("xRes"),
        _T("x texture resolution specified in power of two. Default is 32."),
        wxCMD_LINE_VAL_NUMBER },

      { wxCMD_LINE_OPTION,
        _T("y"), _T("yRes"),
        _T("y texture resolution specified in power of two. Default is 32."),
       wxCMD_LINE_VAL_NUMBER },

      { wxCMD_LINE_OPTION,
        _T("z"), _T("zRes"),
        _T("z texture resolution specified in power of two. Default is 32."),
        wxCMD_LINE_VAL_NUMBER},
       
      { wxCMD_LINE_PARAM,
        0, 0,
        _T("Input directory"),
        wxCMD_LINE_VAL_STRING },
      
     
      {wxCMD_LINE_NONE}
   };
   parser.SetDesc(cmdLineOpts);
   
}
////////////////////////////////////////////////////
bool TCApp::OnCmdLineParsed(wxCmdLineParser& parser)
{
   if(_isBatch){
      _frame = new TCFrame(0,-1,wxT("Texture Creator"));
      _frame->SetMPIVariables( rank, p );
      wxApp::OnCmdLineParsed(parser);
      //set all the options on the translator
      wxString inputDir("./");
      wxString outputDir("./");
      long int resolution[3] = {32,32,32};
      /*if(!parser.Found(wxString("i"),&inputDir)){
         return false;
      }*/
      
      /**/
      inputDir = parser.GetParam(0);
      _frame->SetInputDirectory(inputDir.c_str());
      if(!parser.Found(wxString("y"),&resolution[1])){
        return false;
      }
      if(!parser.Found(wxString("x"),&resolution[0])){
        return false;
      }
      if(!parser.Found(wxString("z"),&resolution[2])){
        return false;
      }
      if(!parser.Found(wxString("o"),&outputDir)){
         _frame->SetOutputDirectory(inputDir.c_str());
      }else{
         _frame->SetOutputDirectory(outputDir.c_str());
      }
      _frame->SetTextureResolution(resolution[0],resolution[1],resolution[2]);
      
   }
   return true;
  
}
