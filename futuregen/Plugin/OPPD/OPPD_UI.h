#ifndef OPPD_UI_H
#define OPPD_UI_H
#include "UIDialog.h"
#include "OPPD_UI_tabs.h"
#include <vector>
#include <string>

using namespace std;

class OPPD_UI_tabs;

class OPPD_UI_Dialog : public UIDialog
{
  DECLARE_DYNAMIC_CLASS(OPPD_UI_Dialog);
 public:
  OPPD_UI_Dialog(wxWindow* parent, int id,
          double* intlinthickdbl,
          double* massfuelburndbl,
          double* solidfuelareadbl,
          double* evalabvfiredbl,
          double* cblburnareadbl,
          long* tempmethod,
          long* tempcalcmethod,
          long* detectortype,
          long* flametype,
          long* detacttemp,
          long* matselindex,
          long* fuelselindex,
          long* vismatselindex,
          long* durmatselindex,
          long* vispropselindex,
          long* viscombselindex,
          long* detrtiselindex,
          long* dettempratselindex,
          long* detspaceselindex,
          long* cableselindex,
		  long* killexcel,
          vector<double>* fuelpardbls,
          vector<double>* compardbls,
          vector<double>* ambpardbls,
          vector<double>* ventpardbls,
          vector<double>* detectpardbls);
  OPPD_UI_Dialog() {};
  
  virtual ~OPPD_UI_Dialog();

  OPPD_UI_tabs* _tabs;
  
  virtual bool TransferDataFromWindow();
  virtual bool TransferDataToWindow();
  virtual void Lock(bool l); 

 public:
  double* p_intlinthickdbl;
  double* p_massfuelburndbl;
  double* p_solidfuelareadbl;
  double* p_evalabvfiredbl;
  double* p_cblburnareadbl;
  long* p_tempmethod;
  long* p_tempcalcmethod;
  long* p_detectortype;
  long* p_flametype;
  long* p_detacttemp;
  long* p_matselindex;
  long* p_fuelselindex;
  long* p_vismatselindex;
  long* p_durmatselindex;
  long* p_vispropselindex;
  long* p_viscombselindex;
  long* p_detrtiselindex;
  long* p_dettempratselindex;
  long* p_detspaceselindex;
  long* p_cableselindex;
  long* p_killexcel;
  vector<double>* p_fuelpardbls;
  vector<double>* p_compardbls;
  vector<double>* p_ambpardbls;
  vector<double>* p_ventpardbls;
  vector<double>* p_detectpardbls;

  void _buildPage();
};

#endif

