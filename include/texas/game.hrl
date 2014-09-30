-record(seat, {
					player,  % player process
					pid,     % player id
					inplay = 0.0,
					bet,     % total bet
					hand,    % cards
					state,   % player state
					muck = false,
					cmd_que = [] % auto-play queue
			 }).

-record(game, {
					gid, 
					type, % game type
					xref = gb_trees:empty(), % key:player,value:seat, easier to retrive seat by player
					seats,
					limit,
					low,
					high,
          			min,
          			max,
					ante,
					deck,  % card deck
					board = [],  % shared cards list
					pot,
					observers = [], 
					timeout, % player timeout
					start_delay, % time to wait players
					raise_count = 0, % number of raises so far
					required_player_count = 2,
					tourney, % tournament info
					timer,
					barrier,
					note
				}).