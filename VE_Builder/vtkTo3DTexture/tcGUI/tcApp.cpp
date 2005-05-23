#include "tcApp.h"
#include <wx/cmdline.h>
IMPLEMENT_APP(TCApp)

//////////////////////////////
//Initialize the application//
//////////////////////////////
bool TCApp::OnInit()
{
   wxApp::OnInit();
   //Create the main window
    _frame = new TCFrame(0,-1,wxT("Texture Creator"));
   if(_isBatch){
      return (_translateFromCmdLine()); 
   }else{
      // Problem with generic wxNotebook implementation whereby it doesn't size
      // properly unless you set the size again
#if defined(__WIN16__) || defined(__WXMOTIF__)
       int width, height;
       _frame->GetSize(& width, & height);
       _frame->SetSize(-1, -1, width, height);
#endif
      //display the UI
      _frame->Show();
      return TRUE;
   }
}
////////////////////////////////////
bool TCApp::_translateFromCmdLine()
{
   
   return true;
}
//////////////////////////////////////////////////
void TCApp::OnInitCmdLine(wxCmdLineParser& parser)
{
   if(argc < 2){
      _isBatch = false;
      return;
   }
   _isBatch = true;
   static const wxCmdLineEntryDesc cmdLineOpts[] = 
   {
      {
       wxCMD_LINE_PARAM,
       "x", "x resolution",
       "x texture resolution specified in power of two.",
       wxCMD_LINE_VAL_NUMBER,wxCMD_LINE_NEEDS_SEPARATOR
      },

      {
       wxCMD_LINE_PARAM,
       "y", "y resolution",
       "y texture resolution specified in power of two.",
       wxCMD_LINE_VAL_NUMBER,wxCMD_LINE_NEEDS_SEPARATOR
      },

      {
       wxCMD_LINE_PARAM,
       "z", "z resolution",
       "z texture resolution specified in power of two.",
       wxCMD_LINE_VAL_NUMBER,wxCMD_LINE_NEEDS_SEPARATOR
      },

      {
       wxCMD_LINE_OPTION,
       "o", "output directory",
       "Directory to write results. Defaults to input directory.",
        wxCMD_LINE_VAL_STRING,wxCMD_LINE_NEEDS_SEPARATOR
      },
      {
       wxCMD_LINE_OPTION,
       "g", "grid type",
       "VTK input dataset grid type. r:Rectilinear s:Structured u:Unstructured",
        wxCMD_LINE_VAL_STRING,wxCMD_LINE_NEEDS_SEPARATOR
      },
      {wxCMD_LINE_NONE}
   };
   parser.SetDesc(cmdLineOpts);
}
////////////////////////////////////////////////////
bool TCApp::OnCmdLineParsed(wxCmdLineParser& parser)
{
   if(_isBatch){
      wxApp::OnCmdLineParsed(parser);
      //set all the options on the translator
      wxString inputDir ;
      wxString outputDir;
      long int resolution[3] = {2,2,2};
      if(!parser.Found(wxString("i"),&inputDir)){
         return false;
      }
      _frame->SetInputDirectory(inputDir.c_str());
      if(!parser.Found(wxString("x"),&resolution[0])){
        return false;
      }
      if(!parser.Found(wxString("y"),&resolution[1])){
        return false;
      }
      if(!parser.Found(wxString("z"),&resolution[2])){
        return false;
      }
      if(!parser.Found(wxString("o"),&outputDir)){
         _frame->SetOutputDirectory(outputDir.c_str());
      }
      _frame->SetTextureResolution(resolution[0],resolution[1],resolution[2]);
      _frame->BatchTranslation();
   }
   return true;
  
}
