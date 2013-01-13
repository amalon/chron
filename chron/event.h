/*
 * chron/event.h
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
 * Class for a chronological event.
 *
 */

#ifndef _CHRON_EVENT_H_
#define _CHRON_EVENT_H_

#include "common.h"
#include "time.h"

namespace chron
{
	class Event
	{
		PlTerm m_event;
		PlTerm m_name;
	public:
		Event()
		{
		}

		Event(PlTerm event)
		: m_event(event)
		{
		}

		Event(PlTerm event, PlTerm name)
		: m_event(event)
		, m_name(name)
		{
		}

		/*
		 * Converters
		 */

		operator const PlTerm &() const
		{
			return m_event;
		}

		/*
		 * Accessors
		 */

		const char *name() const
		{
			return (const char *)(char *)m_name;
		}

		Time operator -(const Event &other) const
		{
			PlTerm t;
			PlCall("event", "get_event_separation",
			       PlTermv(other.m_event, m_event, t));
			return Time(t);
		}
	};
};

#endif // _CHRON_EVENT_H_
