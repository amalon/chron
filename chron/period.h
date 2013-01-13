/*
 * chron/period.h
 *
 * Copyright (C) 2012 James Hogan <james@albanarts.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details
 * (in the file called COPYING).
 *
 *
 * Class for a chronological period.
 *
 */

#ifndef _CHRON_PERIOD_H_
#define _CHRON_PERIOD_H_

#include "common.h"

namespace chron
{
	class Period
	{
		PlTerm m_period;
	public:
		Period(PlTerm period)
		: m_period(period)
		{
		}

		/*
		 * Accessors
		 */

		const char *name() const
		{
			return (const char *)(char *)m_period;
		}

		Event begin()
		{
			PlTerm t;
			PlCall("period", "get_period_begin",
			       PlTermv(m_period, t));
			return Event(t);
		}

		Event end()
		{
			PlTerm t;
			PlCall("period", "get_period_end",
			       PlTermv(m_period, t));
			return Event(t);
		}
	};
};

#endif // _CHRON_PERIOD_H_
