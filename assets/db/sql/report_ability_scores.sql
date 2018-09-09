CREATE VIEW IF NOT EXISTS report_ability_scores AS
SELECT users.userId, users.school, users.grade, users.class,
       first_part_time.createTime AS firstPartTime,
       school_details.province || coalesce (school_details.prefecture, "") || school_details.county AS region,
       school_covers.cover AS school_cover,
       class_covers.cover AS class_cover,
       abilities.name AS ab_name, abilities.name_en AS ab_name_en,
       ability_scores.score
  FROM users
       LEFT JOIN
            (SELECT *
               FROM scores
              WHERE (userId, createTime) IN
                    (SELECT userId, min(createTime)
                       FROM scores
                       GROUP BY userId)) AS first_part_time
       ON users.userId = first_part_time.userId

       LEFT JOIN school_covers
       ON users.school = school_covers.school

       LEFT JOIN school_details
       ON users.school = school_details.school

       LEFT JOIN class_covers
       ON users.school = class_covers.school
          AND users.grade = class_covers.grade
          AND users.class = class_covers.class

       LEFT JOIN ability_scores
       ON users.userId = ability_scores.userId

       LEFT JOIN abilities
       ON ability_scores.abId = abilities.abId;
