CREATE VIRTUAL TABLE fts_idx USING fts5(body, content='db_recipe', content_rowid='id');

-- setup triggers
CREATE TRIGGER db_recipe_ai AFTER INSERT ON db_recipe BEGIN
  INSERT INTO fts_idx(rowid, body) VALUES (new.id, new.body);
END;
CREATE TRIGGER db_recipe_ad AFTER DELETE ON db_recipe BEGIN
  INSERT INTO fts_idx(fts_idx, rowid, body) VALUES('DELETE', old.id, old.body);
END;
CREATE TRIGGER db_recipe_au AFTER UPDATE ON db_recipe BEGIN
  INSERT INTO fts_idx(fts_idx, rowid, body) VALUES('DELETE', old.id, old.body);
  INSERT INTO fts_idx(rowid, body) VALUES (new.id, new.body);
END;
