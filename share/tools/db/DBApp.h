
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

protected:

private:
    ///
    AppFrame* m_appFrame;

};

DECLARE_APP( DBApp )

#endif //DB_APP_H
