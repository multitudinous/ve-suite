#include "GuiVar.h"

IMPLEMENT_DYNAMIC_CLASS(GuiVar, wxObject)

wxString GuiVar::Pack()
{
  wxString result;

  result = char(START);
  result += this->toString();
  result += char(END);
  return result;
};

IMPLEMENT_DYNAMIC_CLASS(GuiInt, GuiVar)

wxString GuiInt::toString()
{
  char buf[50];

  sprintf(buf, "%ld", value);
  return _T(buf);
}


void GuiInt::UnPack(wxString inp)
{
  char buf[50];
  int len;

  strcpy(buf, inp.c_str());
  len= strlen(buf);
  
  buf[len-1]='\0';
  value = atoi(&buf[1]);
 
};

IMPLEMENT_DYNAMIC_CLASS(GuiDouble, GuiVar)

wxString GuiDouble::toString()
{
  char buf[50];

  sprintf(buf, "%.6g", value);
  return _T(buf);
}


void GuiDouble::UnPack(wxString inp)
{
  char buf[50];
  int len;

  strcpy(buf, inp.c_str());
  len= strlen(buf);
  
  buf[len-1]='\0';
  value = atof(&buf[1]);
 
};

IMPLEMENT_DYNAMIC_CLASS(GuiString, GuiVar)

wxString GuiString::toString()
{
  return value;
}

void GuiString::UnPack(wxString inp)
{
  char buf[50];
  int len;
  
  strcpy(buf, inp.c_str());
  len= strlen(buf);
  
  buf[len-1]='\0';
  value = _T(&buf[1]);
}
