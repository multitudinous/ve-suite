#ifndef GUICTX_H
#define GUICTX_H

#include <wx/wx.h>
#include <vector>
#include <string>

#include "GuiVar.h"

using namespace std;

class GuiCtx : public wxObject
{
  DECLARE_DYNAMIC_CLASS(GuiCtx)
 public:
  void RegVar(GuiVar& var); //Register the GuiVariable
  wxString Pack();
  void UnPack(wxString inp);
  
  
 protected:
  vector<GuiVar*> children;
  int get_tokens(wxString inp, char start, char end, vector<wxString> & toks);
};

#endif
