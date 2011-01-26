
// --- QT Includes --- //
#include <QtCore/qpointer.h>

// --- QWT Includes --- //
#include <qwt_series_data.h>

// --- DB Plot Includes --- //
class SignalData;

class CurveData : public QwtSeriesData< QPointF >
{
public:
    ///
    SignalData const& values() const;

    ///
    SignalData& values();

    ///
    virtual QPointF sample( size_t i ) const;

    ///
    virtual size_t size() const;

    ///
    virtual QRectF boundingRect() const;

protected:

private:

};
