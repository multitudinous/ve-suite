#ifndef TEXT_TABLE_H
#define TEXT_TABLE_H
#include "wx/wx.h"
#include "wx/fontenum.h"
#include <vector>


#include "VE_Xplorer/cfdConfig.h"

class WXPLUGIN_DECLSPEC TexTable : public wxTextCtrl
{
 public:

  TexTable(wxWindow* parent, wxWindowID id = -1, const wxPoint& pos = wxDefaultPosition, const wxSize& size = wxDefaultSize);

  ~TexTable() {};
  void SetColWidth(int Col_id, int width);
  void SetNumofCols(int num);

  void AddRow(const std::vector<wxString>& vals);
  int num_cols;
  std::vector<int> cols_width;
  void AddSeperator(char pad='=');
  void DoChangeFont(const wxFont &font);
  bool ChooseFixedFont(int size);
  wxString padding(wxString str, int col_id);
};

#endif
