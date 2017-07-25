/*
 * chron/chron.cpp
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
 * Command line test program of chron library.
 *
 */

#include <SWI-cpp.h>
#include <iostream>
#include <cassert>

#include <chron/chron.h>

static int usage(const char *arg0, int ret)
{
	std::cerr << "Usage: " << arg0 << " <file> [<epoch>]" << std::endl;
	return ret;
}

static bool choose_epoch(chron::Event *event, const char *epoch)
{
	// First try epoch argument
	if (epoch) {
		if (chron::Chronology::lookup_event(event, epoch))
			return true;

		std::cerr << "No event named '" << epoch
			  << "' to use as epoch" << std::endl;
		return false;
	}

	// Failing that, look for an event named "epoch"
	if (chron::Chronology::lookup_event(event, "epoch"))
		return true;

	// Finally fall back to just picking the first event
	if (chron::Chronology::default_epoch(event))
		return true;

	// No events? can't do much with that
	std::cout << "No suitable epoch found" << std::endl;
	return false;
}

int main(int argc, char **argv)
{
	int err;

	if (argc < 2) {
		std::cerr << "No database file provided" << std::endl;
		return usage(argv[0], 1);
	}
	const char *filename = argv[1];

	chron::Database db(argv[0], filename);

	try {
		// get default epoch event
		chron::Event epoch;
		if (!choose_epoch(&epoch, argc >= 3 ? argv[2] : NULL))
			return 1;
		std::cout << "epoch " << epoch.name() << std::endl;

		// load database relative to the epoch
		chron::Chronology db;
		err = db.load(epoch);
		if (err) {
			std::cerr << "load failed" << std::endl;
			return err;
		}
		std::cout << "db loaded" << std::endl;

		// iterate events
		for (chron::Chronology::EventIterator eit(db); eit; ++eit) {
			chron::Event e = *eit;
			chron::Time diff = eit.time();
			std::cout << "event " << e.name() << " = " << diff << std::endl;
		}

		// iterate periods
		for (chron::Chronology::PeriodIterator pit(db); pit; ++pit) {
			chron::Period p = *pit;
			chron::Time begin_diff = pit.begin_time();
			chron::Time end_diff = pit.end_time();
			std::cout << "period " << p.name() << " = (" << begin_diff << ") .. (" << end_diff << ")" << std::endl;
		}
	} catch (PlException e) {
		std::cerr << "Prolog Exception: "
			  << (const char *)e << std::endl;
		return 1;
	}

	return 0;
}
