#include "GuiCtx.h"

IMPLEMENT_DYNAMIC_CLASS(GuiCtx, wxObject);

void GuiCtx::RegVar(GuiVar& var)
{
  children.push_back(&var);
}

wxString GuiCtx::Pack()
{
  wxString result;
  int i;

  for (i=0; i<children.size(); i++)
   result+=children[i]->Pack();
  return result;
}

void GuiCtx::UnPack(wxString inp)
{
  vector<wxString> toks;
  int i, num;

  num=get_tokens(inp, '{', '}', toks);
  if (num!=children.size())
    return;//ERROR;
  for (i=0; i<children.size(); i++)
    children[i]->UnPack(toks[i]);
}


int GuiCtx::get_tokens(wxString inp, char start, char end, vector<wxString> & toks)
{
  char * buf;
  char buf2[1024];
  int len;
  int i, sp;
  
  len = inp.Len();
  buf = new char [len+1];
  
  strcpy(buf, inp.c_str());
  toks.clear();

  for (i=0; i<len; i++)
    {
      if (buf[i]==start)
	{
	  sp=i;
	  buf2[i-sp]=buf[i];
	}
      else if (buf[i]==end)
	{
	  buf2[i-sp]=buf[i];
	  buf2[i-sp+1]='\0';
	  toks.push_back(_T(buf2));
	}
      else
	buf2[i-sp]=buf[i];
    }

  delete [] buf;

  return toks.size();
}
