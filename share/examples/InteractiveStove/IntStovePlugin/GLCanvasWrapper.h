#ifndef GL_CANVAS_WRAPPER_H
#define GL_CANVAS_WRAPPER_H

class GL_Engine;

class wxWindow;
class wxBoxSizer;

class GLCanvasWrapper
{
public:
    GLCanvasWrapper( wxWindow* parent, wxBoxSizer* sizer );
    ~GLCanvasWrapper();
    
    void DrawCanvas();
    void DrawNewBaffle();
    void RemoveBaffle( long int temp1, long int temp2, long int temp3, long int temp4 );
    void RedrawBaffle( long int temp1, long int temp2, long int temp3, long int temp4, int index );

    float* GetGridInfo();
    int* GetGridPoints( int arrayNum );
private:
    GL_Engine* mGLEngine;
    float mGridInfo[ 4 ];
    int mGridPoint1[ 4 ];
    int mGridPoint2[ 4 ];
};
#endif
