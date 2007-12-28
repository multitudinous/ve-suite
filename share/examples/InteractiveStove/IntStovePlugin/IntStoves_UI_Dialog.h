#ifndef IntStoves_UI_DIALOG_H
#define IntStoves_UI_DIALOG_H
#include <wx/spinctrl.h>

#include <ves/conductor/UIDialog.h>

#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/CommandPtr.h>

#include <vector>
#include <string>

class wxComboBox;
class wxButton;
class wxTextCtrl;
class wxStaticBox;
class wxStaticBoxSizer;
class wxBoxSizer;
class wxCheckBox;

namespace ves
{
namespace conductor
{
namespace util
{
   class CORBAServiceList;
}
}
}

class GLCanvasWrapper;

using namespace std;

enum INTERACTIVE_STOVES_IDS
{
      NUMBAFFSEL_COMBOBOX,
	  DESIGN_BUTTON,
	  ACTBAFFSEL_COMBOBOX,
	  ADDBAFF_BUTTON,
	  REMOVEBAFF_BUTTON,
	  REMOVEBAFF_COMBOBOX,
      CHANGE_DEPTH,
      UPDATE_PARAMS,
      VECTOR_CHECKBOX,
      CONTOUR_CHECKBOX
};

class IntStoves_UI_Dialog : public ves::conductor::UIDialog
{
  //DECLARE_DYNAMIC_CLASS(IntStoves_UI_Dialog);
 public:
  IntStoves_UI_Dialog(wxWindow* parent, int id, ves::conductor::util::CORBAServiceList* service,
	      long* numbaffles,
          vector<double>* baffle1,
          vector<double>* baffle2,
          vector<double>* baffle3,
          vector<double>* baffle4,
          vector<double>* baffle5,
          vector<double>* baffle6,
          vector<double>* baffle7);
  IntStoves_UI_Dialog() {};
  
  virtual ~IntStoves_UI_Dialog();
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 
 protected:
  //UI widgets variables
  
 public:
    void _buildPage();
    //void _onNumBafSel(wxCommandEvent& event);
    void _onActBafSel(wxCommandEvent& event);
    //void _onDesignStove(wxCommandEvent& event);
    void _onAddBaff();
    void _onRemoveBaff(wxCommandEvent& event);
    void SetDepth(wxSpinEvent& event);
    void _reDrawBaff(int);
    void _removeBaff(int);
    void _rebuildActBaffSel();
    void _reOrganizeBaffs();
    void SetBaffleData();
    void UpdateParams(wxCommandEvent& event);
    void ShowVectors(wxCommandEvent& event);
    void ShowContour(wxCommandEvent& event);

  long* p_numbaffles;
  vector<double>* p_baffle1;
  vector<double>* p_baffle2;
  vector<double>* p_baffle3;
  vector<double>* p_baffle4;
  vector<double>* p_baffle5;
  vector<double>* p_baffle6;
  vector<double>* p_baffle7;
  //GUI Variables
  wxString baffnums[7];
  wxString activebaff[7];
  bool actbaffdrawn[7];
  vector<double>* temp[7];
    int m_numbaffles;

    int GetStartX(int index);
    int GetStartY(int index);
    int GetDirection(int index);
    int GetLength(int index);
    int GetDepth(int index);
    int GetNumBaffles();
  
    std::vector< double > baffleParams;

    std::vector< ves::open::xml::DataValuePair > parameters;        //The DataValuePairs for the current command

    ves::open::xml::Command* m_command;
    
    void SendCommandsToXplorer();
    void ClearParameters();
    std::string command_name;
    ves::conductor::util::CORBAServiceList* serviceList;

  wxComboBox* _numbaffsel;
  wxComboBox* _activebaffsel;
  wxComboBox* _removebafCombo;
  wxButton* _addbafButton;
  wxButton* _removebafButton;
  wxButton* _updateButton;
  wxButton* m_closeButton;
  wxButton* _designButton;
  GLCanvasWrapper* mCanvasWrapper;
  wxStaticBox* _baff[7];
  wxStaticBoxSizer* _baffGroup[7];
  wxTextCtrl* _startposx[7];
  wxTextCtrl* _startposy[7];
  wxTextCtrl* _direction[7];
  wxTextCtrl* _length[7];
  wxSpinCtrl* _depth[7];
  wxBoxSizer* _rightset;	
  wxBoxSizer* m_buttons;
  wxBoxSizer* m_checkBoxes;
  wxCheckBox* m_vectorCheckBox;
  wxCheckBox* m_contourCheckBox;

  unsigned int vectors;
  unsigned int contour;

  DECLARE_EVENT_TABLE();
};

#endif

