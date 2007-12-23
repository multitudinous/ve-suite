#ifndef GL_ENGINE_H
#define GL_ENGINE_H

#include <wx/glcanvas.h>
#include <GL/glut.h>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <ostream>
#include <stdlib.h>
#include <stdio.h>
#include <vector>

class IntStoves_UI_Dialog;

using namespace std;

class GL_Engine : public wxGLCanvas
{
public:
	GL_Engine(wxWindow* parent, int id, int* attrlist, const wxPoint& pos, const wxSize& size, long style, const wxString& name );
	~GL_Engine();

	void _draw();
	void _checkForMouseHitsOne(int x, int y);
    void _checkForMouseHitsTwo(int x, int y);
	void _drawNewBaffle();
	void _removeBaffle(int, int, int, int);
	void _reDrawBaffle(int, int, int, int, int index);
	void _onMouse(wxMouseEvent& event);
    void OnPaint(wxPaintEvent& event);
    void InletText();
    void OutletText();
    void AddBaffleLabel(float x1, float y1, float x2, float y2, int baffleNum);

    wxGLContext* GetContext();
    wxGLContext* m_glContext;

    IntStoves_UI_Dialog* m_dialog;

    int x, y , direction, length;

	//int* gridptsx;
	//int* gridptsy;
	float gridptsx[49];
	float gridptsy[33];
	int actindex[2];
	int actpt1[2];
	int actpt2[2];

    int xpos2;
    int ypos2;
    int xline;
    int yline;

    int xPoint;
    int yPoint;
    double xLine;
    double yLine;

    double xp;
    double yp;

    bool baffleCreated;

  DECLARE_EVENT_TABLE();
};
#endif
