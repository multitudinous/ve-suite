#ifndef GL_ENGINE_H
#define GL_ENGINE_H

#include "VE_Conductor/Framework/UIDialog.h"
#include <wx/glcanvas.h>
#include <GL/glut.h>
#include <wx/image.h>
#include <wx/wx.h>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <ostream>
#include <stdlib.h>
#include <stdio.h>

using namespace std;

class GL_Engine : public wxGLCanvas
{
public:
	GL_Engine( UIDialog* tcontrol, int id, const wxPoint& pos, const wxSize& size, long style, 
						 const wxString& name );
	~GL_Engine();

	UIDialog* _parent;

	void _draw();
	void _onMouse(wxMouseEvent& event);
	void _drawPoint(int,int);
	void _drawShapes();
	void _drawCircle();
	void _drawRect();

	int activepts;
	int pt1[2];
	int pt2[2];
	float DEG2RAD;
	float radius;


  DECLARE_EVENT_TABLE();
};
#endif
