#ifndef GUIVAR_H
#define GUIVAR_H

#include <wx/wx.h>
#include <wx/object.h>

#define START 123 //ascii of '{'
#define END 125 //ascii of '}'

class GuiVar : public wxObject
{
  DECLARE_DYNAMIC_CLASS(GuiVar)
    
 public:
  
  virtual wxString Pack();
  virtual wxString toString(){ return _T("");};
  virtual void UnPack(wxString){};
};

class GuiInt : public GuiVar
{
  DECLARE_DYNAMIC_CLASS(GuiInt)

 public:
  long value;

  //  virtual wxString Pack();
  virtual wxString toString();
  virtual void UnPack(wxString);
};

class GuiDouble : public GuiVar
{
  DECLARE_DYNAMIC_CLASS(GuiDouble)
 public:
  double value;
  
  //  virtual wxString Pack();
  virtual wxString toString();
  virtual void UnPack(wxString);
};

class GuiString : public GuiVar
{
  DECLARE_DYNAMIC_CLASS(GuiString)
 public:
  wxString value;
  
  //  virtual wxString Pack();
  virtual wxString toString();
  virtual void UnPack(wxString);
};


#endif
