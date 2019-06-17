-- !preview conn=iquizoo_report_db

SELECT
  course_result_orginal.UserId AS user_id,
  iquizoo_content_db.evaluation_games.Name AS game_name,
  course_result_orginal.CreateTime AS part_time,
  course_result_orginal.OrginalData AS game_data,
  course_result_orginal.TimeConsuming AS game_duration
FROM
  course_result_orginal
  INNER JOIN iquizoo_content_db.evaluation_games ON
             iquizoo_content_db.evaluation_games.Id = course_result_orginal.ContentId
WHERE
  course_result_orginal.OrganizationId IN ( {customer_projectids} );
