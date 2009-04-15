
// --- VE-Suite Includes --- //
#ifndef DB_APP_H
#define DB_APP_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/app.h>

/*!\file DBApp.h
 * DBApp API
 */

/*!\class DBApp
 *
 */
class DBApp : public wxApp
{
public:
    ///
    virtual bool OnInit();
    
    ///
    virtual int OnExit();

    AppFrame* m_appFrame;

protected:

private:
    
};

DECLARE_APP( DBApp )

#endif //DB_APP_H
