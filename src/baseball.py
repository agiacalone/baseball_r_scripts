"""
baseball.py — shared helper functions for MLB stats notebooks.
Python port of the R/baseballr project.
"""

import statsapi
import pandas as pd
from datetime import date, timedelta
from zoneinfo import ZoneInfo
from pathlib import Path
from IPython.display import Markdown, display

# ---------------------------------------------------------------------------
# Global variables (mirrors baseball_games.r)
# ---------------------------------------------------------------------------

SEASON = 2025
PACIFIC = ZoneInfo("America/Los_Angeles")

today     = date.today()
yesterday = today - timedelta(days=1)
tomorrow  = today + timedelta(days=1)

DIVISION_NAMES = {
    200: "AL West",
    201: "AL East",
    202: "AL Central",
    203: "NL West",
    204: "NL East",
    205: "NL Central",
}

# ---------------------------------------------------------------------------
# Private helpers
# ---------------------------------------------------------------------------

def _fmt_date(d) -> str:
    """Convert a date object or ISO string to MM/DD/YYYY (statsapi format)."""
    if isinstance(d, str):
        d = date.fromisoformat(d)
    return d.strftime("%m/%d/%Y")


def _season_schedule_raw(season: int) -> list:
    """Fetch the full season schedule as a raw list of dicts."""
    return statsapi.schedule(
        start_date=f"03/01/{season}",
        end_date=f"11/30/{season}",
        sportId=1,
    )


def _parse_schedule_df(games: list) -> pd.DataFrame:
    """
    Normalize the raw list-of-dicts from statsapi.schedule() into a clean
    DataFrame. Mirrors the select/mutate/arrange pattern in season_games.R,
    team_games.R, and one_day_games.R.
    """
    EMPTY_COLS = ["date", "time", "game_pk", "awayTeamName", "homeTeamName",
                  "awayScore", "homeScore", "series_description"]

    if not games:
        return pd.DataFrame(columns=EMPTY_COLS)

    df = pd.DataFrame(games)

    # Parse UTC datetimes and convert to Pacific (mirrors lubridate::with_tz)
    df["game_datetime"] = pd.to_datetime(df["game_datetime"], utc=True)
    df["game_datetime"] = df["game_datetime"].dt.tz_convert("America/Los_Angeles")
    df["date"] = df["game_datetime"].dt.date
    df["time"] = df["game_datetime"].dt.strftime("%H:%M:%S")

    df = df.rename(columns={
        "game_id":            "game_pk",
        "away_name":          "awayTeamName",
        "home_name":          "homeTeamName",
        "away_score":         "awayScore",
        "home_score":         "homeScore",
        "series_description": "series_description",
    })

    keep = [c for c in EMPTY_COLS if c in df.columns]
    return df[keep].sort_values(["date", "time"]).reset_index(drop=True)


# ---------------------------------------------------------------------------
# Schedule functions
# ---------------------------------------------------------------------------

def game_ident(team_id: int, game_date) -> int | None:
    """
    Return the game_pk for a specific team on a specific date.
    Returns None if no game found. Mirrors game_ident.R.
    Note: returns the first game only (doubleheader game 1 matches R behavior).
    """
    date_str = _fmt_date(game_date)
    try:
        games = statsapi.schedule(
            start_date=date_str,
            end_date=date_str,
            team=team_id,
            sportId=1,
        )
    except Exception as e:
        print(f"Error fetching game_ident for team {team_id} on {game_date}: {e}")
        return None

    if not games:
        return None
    return games[0].get("game_id")


def gameinfo(game_pk) -> dict:
    """Raw game info dict. Mirrors gameinfo.R."""
    if not game_pk:
        return {}
    try:
        return statsapi.get("game", {"gamePk": game_pk})
    except Exception as e:
        print(f"Error fetching game info for {game_pk}: {e}")
        return {}


def season_games(season: int) -> pd.DataFrame:
    """All games for a full season. Mirrors season_games.R."""
    try:
        games = _season_schedule_raw(season)
    except Exception as e:
        print(f"Error fetching season schedule: {e}")
        return pd.DataFrame()
    return _parse_schedule_df(games)


def team_games(team_id: int, season: int) -> pd.DataFrame:
    """All games for a team in a season. Mirrors team_games.R."""
    try:
        games = statsapi.schedule(
            start_date=f"03/01/{season}",
            end_date=f"11/30/{season}",
            team=team_id,
            sportId=1,
        )
    except Exception as e:
        print(f"Error fetching team games: {e}")
        return pd.DataFrame()
    return _parse_schedule_df(games)


def one_day_games(game_date, season: int) -> pd.DataFrame:
    """All games on a given date. Mirrors one_day_games.R."""
    date_str = _fmt_date(game_date)
    try:
        games = statsapi.schedule(start_date=date_str, end_date=date_str, sportId=1)
    except Exception as e:
        print(f"Error fetching games for {game_date}: {e}")
        return pd.DataFrame()
    return _parse_schedule_df(games)


def date_game(game_pk, season: int) -> pd.DataFrame:
    """Schedule info for a single game by game_pk. Mirrors date_game.R."""
    EMPTY_COLS = ["date", "time", "game_pk", "awayTeamName", "homeTeamName",
                  "awayScore", "homeScore", "series_description"]
    if not game_pk:
        print("No valid game_pk provided. Returning empty DataFrame.")
        return pd.DataFrame(columns=EMPTY_COLS)
    try:
        games = statsapi.schedule(game_id=game_pk)
    except Exception as e:
        print(f"Error fetching game {game_pk}: {e}")
        return pd.DataFrame(columns=EMPTY_COLS)
    return _parse_schedule_df(games)


# ---------------------------------------------------------------------------
# Box score
# ---------------------------------------------------------------------------

def make_box_score(team_id: int, game_date) -> pd.DataFrame:
    """
    Line score by inning + R/H/E totals. Mirrors make_box_score.R.
    Returns empty DataFrame if no game or no linescore data.
    """
    EMPTY = pd.DataFrame(columns=["Inning", "Away", "Home"])

    gid = game_ident(team_id, game_date)
    if not gid:
        print(f"No game found for team {team_id} on {game_date}. Skipping.")
        return EMPTY

    try:
        linescore = statsapi.get("game_linescore", {"gamePk": gid})
    except Exception as e:
        print(f"Error fetching linescore for game {gid}: {e}")
        return EMPTY

    if not linescore or "innings" not in linescore:
        return EMPTY

    innings_data = linescore.get("innings", [])
    if not innings_data:
        return EMPTY

    teams_data = linescore.get("teams", {})
    away_info  = teams_data.get("away", {})
    home_info  = teams_data.get("home", {})
    away_name  = away_info.get("team", {}).get("name", "Away")
    home_name  = home_info.get("team", {}).get("name", "Home")

    rows = []
    for inn in innings_data:
        rows.append({
            "Inning":  str(inn.get("num", "?")),
            away_name: str(inn.get("away", {}).get("runs", 0)),
            home_name: str(inn.get("home", {}).get("runs", 0)),
        })

    # R/H/E summary rows
    for label, key in [("R", "runs"), ("H", "hits"), ("E", "errors")]:
        rows.append({
            "Inning":  label,
            away_name: str(away_info.get(key, 0)),
            home_name: str(home_info.get(key, 0)),
        })

    return pd.DataFrame(rows)


# ---------------------------------------------------------------------------
# Play-by-play
# ---------------------------------------------------------------------------

_PBP_SUMMARY_COLS = [
    "at_bat", "pitch_in_ab", "inning", "half", "pitchnum",
    "awayscore", "homescore", "outs", "detail",
    "batter", "pitcher", "description",
]

_PBP_HALF_COLS = [
    "inning", "half", "at_bat", "pitch_in_ab",
    "awayscore", "homescore", "outs",
    "batter", "pitcher", "description",
]


def pbp_summary(game_pk) -> pd.DataFrame:
    """
    Play-by-play, one row per pitch event. Mirrors pbp_summary.R.
    Flattens allPlays -> playEvents from the MLB API.
    """
    empty = pd.DataFrame(columns=_PBP_SUMMARY_COLS)

    if not game_pk:
        return empty

    try:
        raw = statsapi.get("game_playByPlay", {"gamePk": game_pk})
    except Exception as e:
        print(f"Error fetching play-by-play for game {game_pk}: {e}")
        return empty

    all_plays = (raw or {}).get("allPlays", [])
    if not all_plays:
        return empty

    rows = []
    for play in all_plays:
        about   = play.get("about", {})
        matchup = play.get("matchup", {})
        count   = play.get("count", {})
        batter  = matchup.get("batter",  {}).get("fullName", "Unknown")
        pitcher = matchup.get("pitcher", {}).get("fullName", "Unknown")

        for event in play.get("playEvents", []):
            details = event.get("details", {})
            rows.append({
                "at_bat":      about.get("atBatIndex"),
                "pitch_in_ab": event.get("index"),
                "inning":      about.get("inning"),
                "half":        about.get("halfInning"),
                "pitchnum":    event.get("pitchNumber"),
                "awayscore":   details.get("awayScore"),
                "homescore":   details.get("homeScore"),
                "outs":        count.get("outs"),
                "detail":      details.get("code"),
                "batter":      batter,
                "pitcher":     pitcher,
                "description": details.get("description"),
            })

    if not rows:
        return empty

    df = pd.DataFrame(rows, columns=_PBP_SUMMARY_COLS)
    df["half"] = pd.Categorical(df["half"], categories=["top", "bottom"], ordered=True)
    return df.sort_values(["at_bat", "pitch_in_ab"]).reset_index(drop=True)


def pbp_half(game_pk) -> pd.DataFrame:
    """
    Play-by-play for half-inning grouping. Mirrors pbp_half.R.
    Reuses pbp_summary and selects the relevant columns.
    """
    empty = pd.DataFrame(columns=_PBP_HALF_COLS)

    if not game_pk:
        return empty

    full = pbp_summary(game_pk)
    if full.empty:
        return empty

    df = full[_PBP_HALF_COLS].copy()
    df["half"] = pd.Categorical(df["half"], categories=["top", "bottom"], ordered=True)
    return df.sort_values(["inning", "half", "at_bat", "pitch_in_ab"]).reset_index(drop=True)


def grouped(pbp_data: pd.DataFrame) -> list:
    """
    Split PBP DataFrame into a list of per-half-inning DataFrames.
    Mirrors grouped.R (group_split by inning, half).
    """
    if pbp_data is None or pbp_data.empty:
        return []

    missing = {"inning", "half"} - set(pbp_data.columns)
    if missing:
        print(f"Warning: Missing required columns: {missing}")
        return []

    result = []
    for _, grp in pbp_data.groupby(["inning", "half"], observed=True, sort=True):
        result.append(grp.reset_index(drop=True))
    return result


def text_recap(pbp_data: pd.DataFrame) -> list:
    """
    Prose-style recap lines. Mirrors text_recap.R.
    Returns a list of strings; join with '\\n' or print() each.
    """
    if pbp_data is None or pbp_data.empty:
        return ["No play-by-play data available for this game."]

    try:
        half_groups = grouped(pbp_data)
    except Exception as e:
        print(f"Error grouping play-by-play data: {e}")
        return ["No play-by-play data available for this game."]

    if not half_groups:
        return ["No play-by-play data available for this game."]

    lines = []
    for grp in half_groups:
        if grp.empty:
            continue
        inning = grp["inning"].iloc[0] if "inning" in grp.columns else "?"
        half   = str(grp["half"].iloc[0]).title() if "half" in grp.columns else "?"
        lines.append(f"\n--- {half} of Inning {inning} ---\n")

        for _, row in grp.iterrows():
            batter  = row.get("batter",      "Unknown")
            pitcher = row.get("pitcher",     "Unknown")
            desc    = row.get("description", "No description")
            lines.append(f"Batter: {batter} | Pitcher: {pitcher}\n  {desc}")

    return lines


# ---------------------------------------------------------------------------
# Output helpers
# ---------------------------------------------------------------------------

def output_markdown(df: pd.DataFrame, file: str = None) -> None:
    """
    Write DataFrame as a markdown table to file and display inline in notebook.
    Mirrors output_markdown.R. Pass file=None to display only (no file write).
    """
    if df is None or (hasattr(df, "empty") and df.empty):
        md = "_No data available._"
    else:
        md = df.to_markdown(index=False)

    if file:
        path = Path(file)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(md + "\n", encoding="utf-8")
        print(f"Markdown table written to {file}")

    display(Markdown(md))


def write_half_inning_tables(grouped_list: list, file: str = "half_innings.md") -> None:
    """
    Write per-half-inning markdown tables to a file and display inline.
    Mirrors write_half_inning_tables.R.
    """
    output_lines = []

    if not grouped_list:
        output_lines.append("No half-inning data available.")
    else:
        for grp in grouped_list:
            if grp is None or grp.empty:
                continue
            inning = grp["inning"].iloc[0] if "inning" in grp.columns else "?"
            half   = grp["half"].iloc[0]   if "half"   in grp.columns else "?"
            output_lines.append(f"\n### Inning {inning} ({half})\n")
            output_lines.append(grp.to_markdown(index=False))
            output_lines.append("")

        if not output_lines:
            output_lines.append("No half-inning data available.")

    full_text = "\n".join(output_lines)
    path = Path(file)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(full_text + "\n", encoding="utf-8")
    print(f"Markdown table written to {file}")
    display(Markdown(full_text))


# ---------------------------------------------------------------------------
# Standings
# ---------------------------------------------------------------------------

def _standings_raw_to_df(league_id: int, season: int,
                          standings_type: str = "regularSeason") -> pd.DataFrame:
    """
    Normalize statsapi.standings_data() output into a flat DataFrame.
    Internal helper used by all standings functions.
    """
    try:
        raw = statsapi.standings_data(
            leagueId=str(league_id),
            season=str(season),
            standingsTypes=standings_type,
        )
    except Exception as e:
        print(f"Error fetching standings: {e}")
        return pd.DataFrame()

    if not raw:
        return pd.DataFrame()

    rows = []
    for div_id, div_data in raw.items():
        div_name = DIVISION_NAMES.get(int(div_id), str(div_id))
        for team in div_data.get("teams", []):
            rows.append({
                "Division": div_name,
                "Rank":     team.get("div_rank", "?"),
                "Team":     team.get("name",      "?"),
                "Wins":     team.get("w",           0),
                "Losses":   team.get("l",           0),
                "GB":       team.get("gb",          "-"),
                "WLPct":    team.get("pct",         ".000"),
            })

    return pd.DataFrame(rows) if rows else pd.DataFrame()


def standings(season: int, league_id: int) -> pd.DataFrame:
    """Division standings for one league. Mirrors standings.R."""
    df = _standings_raw_to_df(league_id, season)
    if df.empty:
        return df
    return (df
            .sort_values(["Division", "Rank", "WLPct"],
                         ascending=[True, True, False])
            .reset_index(drop=True))


def al_nl_combined_standings(season: int) -> pd.DataFrame:
    """AL + NL combined, sorted by winning pct. Mirrors al_nl_combined_standings.R."""
    al = _standings_raw_to_df(103, season)
    nl = _standings_raw_to_df(104, season)
    df = pd.concat([al, nl], ignore_index=True)
    if df.empty:
        return df
    df = (df
          .sort_values(["WLPct", "Rank"], ascending=[False, True])
          .reset_index(drop=True))
    df.insert(0, "OverallRank", range(1, len(df) + 1))
    return df


def combined_intraleague_standings(season: int, league_id: int) -> pd.DataFrame:
    """All divisions in one league merged and sorted by WLPct. Mirrors combined_intraleague_standings.R."""
    df = _standings_raw_to_df(league_id, season)
    if df.empty:
        return df
    df = (df
          .sort_values(["WLPct", "Rank"], ascending=[False, True])
          .reset_index(drop=True))
    df.insert(0, "OverallRank", range(1, len(df) + 1))
    return df


def wildcard_standings(season: int, league_id: int) -> pd.DataFrame:
    """Wildcard standings for one league. Mirrors wildcard_standings.R."""
    df = _standings_raw_to_df(league_id, season, standings_type="wildCard")
    if df.empty:
        return df
    df = (df
          .sort_values(["Rank", "WLPct"], ascending=[True, False])
          .reset_index(drop=True))
    df.insert(0, "OverallRank", range(1, len(df) + 1))
    wc_map = {"AL East": "AL Wild", "NL East": "NL Wild"}
    df["Division"] = df["Division"].map(lambda x: wc_map.get(x, x))
    return df


# ---------------------------------------------------------------------------
# Team IDs
# ---------------------------------------------------------------------------

def team_ids(season: int) -> pd.DataFrame:
    """All MLB teams with IDs and names. Mirrors get_team_ids.R."""
    try:
        raw = statsapi.get("teams", {"sportId": 1, "season": season})
    except Exception as e:
        print(f"Error fetching team list: {e}")
        return pd.DataFrame()

    teams_list = (raw or {}).get("teams", [])
    rows = []
    for t in teams_list:
        lg_id = t.get("league", {}).get("id")
        if lg_id not in (103, 104):
            continue
        rows.append({
            "team_id":           t.get("id"),
            "team_abbreviation": t.get("abbreviation"),
            "teamname":          t.get("name", "").title(),
            "league_id":         lg_id,
        })

    if not rows:
        return pd.DataFrame()
    return pd.DataFrame(rows).sort_values("team_id").reset_index(drop=True)
