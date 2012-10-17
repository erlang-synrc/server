-record(feed, {id,
               top,
               aclver}).

%%FIX: will be extended with numerous new fields
-record(entry, {id, %% {entry_id, feed_id}
                entry_id, %% taken from id_seq
                feed_id, %% owner's feed_id, indexed field
                from, %% author
                to,
                description,
                raw_description,
                created_time,
                hidden,
                access,
                shared,
                starred,
                deleted,
                likes,
                likes_count,
                comments,
                comments_rear,
                comments_count,
                media = [], %% for oembed
                etc,        %% field to link additional info
                type = {user, normal},
                next,
                prev
               }).

-record(id_seq, {thing, %% feed, user, group, ...
                 id = 100000}).


-record(media, {id,
		title :: iolist(),
                width,
                height,
                html :: iolist(),
                url :: iolist(),
                version,
                thumbnail_url :: iolist(),
                type :: {atom(), atom() | string()},
                thumbnail_height}).

-record(comment, {id,          %% {comment_id, entry_id}
                  comment_id,  %% generowane przez id_seq
                  entry_id,    %% index
                  raw_content, %% raw text of comment
                  content,     %% text of comment
                  author_id,
                  create_time,
                  media = [],  %% for oembed
                  parent,
                  comments,
                  comments_rear,
                  next,
                  prev
                 }).


-record(entry_likes, {entry_id,       % this is a general entry_id. Every same entry in different feeds has the same id
                one_like_head,  % this is a head for linked list of {user, time} tupples
                total_count     % it's easier to keep it than count
                }).

-record(user_likes, {user_id,
                one_like_head,
                total_count
                }).

-record(one_like, {id,              % just a number
                    user_id,        % who likes
                    entry_id,       % what
                    feed_id,        % where
                    created_time,   % when
                    next            
                }).


% Statistics. We have to keep count of user entries and comments. 
% Gathering it the old way will work very ineffective with more users to come.
% And comments from user record are somehow always undefined. Either it fails, or it is used somewhere else

-record(user_etries_count, {
        user_id,    % user id
        entries = 0,    % number of entries
        comments = 0   % number of comments
    }).
